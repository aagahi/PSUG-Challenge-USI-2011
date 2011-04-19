package org.psug.usi.service

import org.psug.usi.domain._
import collection.mutable.HashMap
import akka.util.Logging
import org.psug.usi.akka.Receiver
import java.util.concurrent.TimeUnit
import org.psug.usi.store._
import org.psug.usi.twitter.Twitter
import akka.actor.{Actor, Channel, Scheduler}
import java.util.Properties

/**
 * User: alag
 * Date: 2/17/11
 * Time: 11:32 PM
 */

case class InitGame(game: Game)
//reply for game init
sealed trait InitGameReply
case object InitGameSuccess extends InitGameReply
case object ErrorAlreadyStarted extends InitGameReply

case class Register(user: User)
//reply for game Register
sealed trait RegisterReply
case object RegisterSuccess extends RegisterReply
case object RegisterFailure extends RegisterReply

case object QueryStats
case object GameManagerError

case class GameManagerStats(registredPlayer: Int, currentQuestionPlayersCount: Int, state:GameState )

case class QueryQuestion(userId: Int, questionIndex: Int)

case class UserAnswer(userId: Int, questionIndex: Int, answerIndex: Int)

case class QueryScoreSlice(userId: Int)
case class QueryScoreSliceAudit( userEmail: String)

//answer to QueryScoreSlice may be "don't ask" if the game is not finished
trait ScoreSliceAnswer
case object ScoreSliceUnavailable extends ScoreSliceAnswer
case class ScoreSlice(r:RankingVO) extends ScoreSliceAnswer


case class QueryHistory( userEmail:String, questionIndex:Option[Int])
case class GameAnwsersHistory(answers:AnswersHistoryVO)
case class GameAnwserHistory(answer:AnswerHistoryVO)


case class UserAnswerResponse(answerStatus: Boolean, correctAnwser:String, score: Int)
case class UserAnswerResponseVO(are_u_right: Boolean, good_answer:String, score:Int )



case class QuestionResponse(question: Question, score:Int)

object TimeoutType extends Enumeration {
  val LOGIN, SYNCRO, QUESTION = Value
}

case class TimeoutMessage(timeoutType: TimeoutType.Value, questionIndex: Int, timoutSec: Int)

trait GameManagerTimer {
  def schedule(questionTimeout: TimeoutMessage, target:Receiver)
}

class DefaultGameManagerTimer extends GameManagerTimer {
  def schedule(questionTimeout: TimeoutMessage, target:Receiver) {
    Scheduler.scheduleOnce( target.actorRef, questionTimeout, questionTimeout.timoutSec, TimeUnit.SECONDS )
  }
}


/**
 * https://sites.google.com/a/octo.com/challengeusi2011/l-application-de-quiz/sequences-d-appels
 * A game can be:
 * - not defined (started but not initialized)
 * - started and initialized (waiting for user login)
 * - started and waiting for answer X
 * - finished and processing score
 * - finished and awaiting for score queries
 */
trait GameState
case object Uninitialized extends GameState
//init done, and no user requested /api/login yet
case object Initialized extends GameState 
case object WaitingRegistrationAndQ1 extends GameState
//could be a couple of ProcessingQueryQuestion(i) / ProcessingReplyQuestion(i)
//in place of GameManager.currentQuestionIndex
case object InGame extends GameState 
//case class ProcessingQueryQuestion(number:Int) extends GameState
//case class ProcessingReplyQuestion(number:Int) extends GameState
case object EndGame extends GameState

/**
 * A game manager: handle question/anwser and timeout
 */
class GameManager( services:Services,
                   timer: GameManagerTimer = new DefaultGameManagerTimer)
  extends Receiver with Logging {
  
  import services._

  var gameState : GameState = Uninitialized
  var game: Game = _
  var scorer:Scorer = _

  class QuestionPlayer {
    var playerIndex = 0
    val players = new Array[Int](game.nbUsersThreshold)
    val playerActors = new HashMap[Int, Channel[Any]]
  }

  var currentQuestionIndex = 0
  var userAnswerCount = 0

  var currentQuestionPlayer : QuestionPlayer = null
  var nextQuestionPlayer : QuestionPlayer = null

  val registredPlayersHistory = new HashMap[Int, UserAnswerHistory]

  // TODO: add trycatch to avoid actor crash
  def receive = {
    case InitGame(game) => initGame(game)
    case Register(user) => register(user)
    case QueryStats => 
      sender ! GameManagerStats(registredPlayersHistory.size, currentQuestionPlayer.playerIndex,gameState)
    case QueryQuestion(userId, questionIndex) 
      if( questionIndex >= currentQuestionIndex && questionIndex <= currentQuestionIndex+1 && registredPlayersHistory.isDefinedAt(userId) )
      => queryQuestion(userId, questionIndex)
    case UserAnswer(userId, questionIndex, answerIndex) 
      if (registredPlayersHistory.isDefinedAt(userId) && questionIndex == currentQuestionIndex)
      => answer(userId, answerIndex)
    case TimeoutMessage(timeoutType, questionIndex, timeoutSec) 
      if (questionIndex == currentQuestionIndex) 
      => timeout(timeoutType)
    case QueryScoreSlice(userId) => queryScoreSlice(userId)
    case QueryScoreSliceAudit(userEmail) => queryScoreSliceAudit(userEmail)
    case QueryHistory( userEmail, questionIndex ) => queryGameHistoryAudit( userEmail, questionIndex )

    case msg:RepositoryCleared => // noop
      
    case x =>
      log.warn( "Unhandled GameManager message: " + x )
      sender ! GameManagerError
  }


  /**
   * Initialize a game (works as reset)
   */
  private def initGame(game: Game): Unit = {
    // game state check removed for simplification we assume that init reset the whole game state
    this.game = game
    scorer = new Scorer()
    currentQuestionIndex = 0
    registredPlayersHistory.clear()
    currentQuestionPlayer = new QuestionPlayer
    nextQuestionPlayer = new QuestionPlayer
    gameState = Initialized
    if( game.flushUserTable ){
      gameUserHistoryService ! ClearRepository
      userRepositoryService ! ClearRepository
    }
    sender ! InitGameSuccess
  }

  /**
   * Register an user for the game (user login). 
   * A game has a max number of user, and a timer is started after
   * the first query for login. 
   * After login and login ack, user must ask the first question.
   * When the timer is expired or the number of user is reached 
   * and they all asked for the first question
   * (first one met), the game reply with the Question 1.
   */
  private def register(user:User) {
    def tryToAddUser {
      if (registredPlayersHistory.size < game.nbUsersThreshold) {
       if(!registredPlayersHistory.isDefinedAt(user.id)) {
         registredPlayersHistory(user.id) = UserAnswerHistory( user, 0, Nil )
         sender ! RegisterSuccess
       } else { // user is already registered
         sender ! RegisterFailure
       }
      } else {
        sender ! RegisterFailure
      }
    }
    
    gameState match {
      case Initialized =>
        // initialize the logintimeout timer for long polling
        timer.schedule( TimeoutMessage(TimeoutType.LOGIN, currentQuestionIndex, game.loginTimeoutSec), this )
        this.gameState = WaitingRegistrationAndQ1
        //add that user
        tryToAddUser
      case WaitingRegistrationAndQ1 =>
        tryToAddUser
      case _ => 
        sender ! RegisterFailure
    }
  }

  /**
   * A user query a question. If the question number is the currently processed, 
   * we wait for all user to query that question and then reply to all of them.
   * questionIndex start at 0 (fyi octo rest api assume it starts at 1 => http request handler should have set -1)
   */
  private def queryQuestion(userId: Int, questionIndex: Int) {
    //TODO: why not check that the user is in that game ?
    
    //TODO: I don't see a case in the spec where a user is allowed to
    //query for a question that is not currentQuestionIndex
    val questionPlayer = if (questionIndex > currentQuestionIndex) nextQuestionPlayer else currentQuestionPlayer


    questionPlayer.players(questionPlayer.playerIndex) = userId

    questionPlayer.playerIndex += 1
    questionPlayer.playerActors(userId) = sender

    if (nextQuestionPlayer.playerIndex >= game.nbUsersThreshold ) {
      gameState match {
        case InGame =>
          currentQuestionPlayer = nextQuestionPlayer
          nextQuestionPlayer = new QuestionPlayer
          //TODO: I thing that there is always a SynchroTime, even if all users asked for the question
          //before QuestionTimeFrame expiration - see "sequences-d-appels", les question, cas 1
          currentQuestionIndex += 1
          replyQuestion()

        case _ =>
          log.error("TODO: error message: Not supposed to query question in current game state " + gameState )
      }
    }
    else if (questionIndex == currentQuestionIndex && currentQuestionPlayer.playerIndex >= game.nbUsersThreshold ) {
      gameState match {
        case WaitingRegistrationAndQ1 =>
          gameState = InGame
          replyQuestion()
        case InGame =>
          replyQuestion()
        case _ =>
          log.error("TODO: error message: Not supposed to query question in current game state " + gameState )
      }

    }
  }

  /**
   * A registered user for that game answer for the current
   * question
   */
  private def answer(userId: Int, answerIndex: Int) {
    userAnswerCount += 1
    val userAnswerHistory = registredPlayersHistory( userId )

    userAnswerHistory.answersHistory = AnswerHistory(currentQuestionIndex, answerIndex) :: userAnswerHistory.answersHistory

    val currentQuestion = game.questions(currentQuestionIndex)


    val answerValue = if (currentQuestion.answers(answerIndex).status){
        val bonus = userAnswerHistory.answerBonus
        userAnswerHistory.answerBonus += 1
        currentQuestion.value + bonus
      } else {
        userAnswerHistory.answerBonus = 0
        0
      }

    val userScore = scorer.scoreAnwser(ScorerAnwserValue(userAnswerHistory.user, answerValue))

    sender ! UserAnswerResponse( answerValue > 0, game.correctAnswer(currentQuestionIndex), userScore.score)


    if (currentQuestionIndex == game.questions.size - 1 && userAnswerCount == game.nbUsersThreshold ) {
      endGame()
    }

  }

  /**
   * Query for the ranking of the given user.
   * Can be done only when EndGame state is reached 
   * (last question answered and SynchroTime expired)
   * 
   * Reply Some(List(scores)) if score are available
   * or None if it is not the time to ask
   */
  private def queryScoreSlice(userId: Int) {
    gameState match {
      case EndGame =>
        sender ! ScoreSlice(scorer.scoreSlice( registredPlayersHistory(userId).user ))
      case _ => 
        log.error("Error: ask for the score or ranking on a non finished game")
        sender ! ScoreSliceUnavailable
    }
  }


  /*
    Restore last game and scorer in case of VM restart
   */
  private def restoreLastGameState(){
    gameState match {
      case Uninitialized => // need to reload last game
        (gameRepositoryService !? PullLast) match {
          case DataPulled( Some( lastGame ) ) =>
            this.game = lastGame.asInstanceOf[Game]
            scorer = new Scorer()
            scorer.load( game.id )
            gameState = EndGame

          case _ => 
        }

      case EndGame =>  // no need to reload

      case _ => log.warn( "Unexpected restore request")
    }

  }

  /**
   * Audit query for the ranking of the given user.
   * Reply Some(List(scores)) if score are available
   * or None if it is not the time to ask
   */
  private def queryScoreSliceAudit(userEmail: String) {
    restoreLastGameState()

    gameState match {
      case EndGame => 
        val target = sender
        userRepositoryService.callback( PullDataByEmail( userEmail ) ){
          case DataPulled( Some( data ) ) =>
            target ! ScoreSlice( scorer.scoreSlice( data.asInstanceOf[User] ) )

          case _ =>
            log.warn( "Unexpected repo result for user email " + userEmail )
            target ! GameManagerError
        }
      case _ => 
        log.error("Error: ask for the score or ranking on a non finished game")
        sender ! ScoreSliceUnavailable
    }
  }

  /**
   * Audit query for user game history
   * Reply Answers if avalaible
   * or None if it is not the time to ask
   */
  private def queryGameHistoryAudit(userEmail: String, questionIndex:Option[Int]) {
    gameState match {
      case EndGame => 
        val target = sender
        userRepositoryService.callback( PullDataByEmail( userEmail ) ){
          case DataPulled( Some( data ) ) =>
            val key = GameUserKey( game.id, data.asInstanceOf[User].id )
            gameUserHistoryService.callback( PullData( key ) ){
              case DataPulled( Some( data ) ) =>
                val gameUserHistory = data.asInstanceOf[GameUserHistory]
    
                questionIndex match {
                  case Some( questionIndex ) =>
    
                    val answerVO = AnswerHistoryVO(  user_answer = gameUserHistory.anwsers.find( _.questionIndex == questionIndex ).map( _.answerIndex + 1 ).getOrElse(0),
                                                     good_answer = game.correctAnswerIndex( questionIndex  ) + 1,
                                                     question = game.questions( questionIndex ).question )
                    target ! GameAnwserHistory( answerVO )
    
                  case None =>
                    val s = game.questions.size
                    val answersVO = AnswersHistoryVO( new Array(s), new Array(s) )
                    for( i <- 0 until s ){
                      // here again we assume anwser number starting at 1 (instead of 0)
                      answersVO.user_answers(i) = gameUserHistory.anwsers.find( _.questionIndex == i ).map( _.answerIndex + 1 ).getOrElse(0)
                      // here again we assume anwser number starting at 1 (instead of 0)
                      answersVO.good_answers(i) = game.correctAnswerIndex( i ) + 1
                    }
                    target ! GameAnwsersHistory( answersVO )
                }
              case _ =>
                log.warn( "Unexpected repo result for GameUserHistory " + key  )
                target ! GameManagerError
    
            }
    
          case _ =>
            log.warn( "Unexpected repo result for User email " + userEmail )
            target ! GameManagerError
        }
        
      case _ =>
        log.error("Error: ask for the score or ranking on a non finished game")
        sender ! ScoreSliceUnavailable
    }
  }


  /**
   * Send the text of the current question to all users who asked for.
   * User who didn't asked for the Question N at that time won't be able
   * to answer it
   * @return
   */
  private def replyQuestion() {
    userAnswerCount = 0
    val question =game.questions(currentQuestionIndex)
    currentQuestionPlayer.playerActors.foreach {
      case (userId, playerActor) =>
        playerActor ! QuestionResponse(question, scorer.userScore( userId ) )
    }
    currentQuestionPlayer.playerActors.clear
    timer.schedule( TimeoutMessage(TimeoutType.QUESTION, currentQuestionIndex, game.questionTimeFrameSec), this )
  }

  /**
   * End the game
   * Store all user answer history
   * @return
   */
  private def endGame(){
    gameState match {
      case InGame =>
        log.info("Ending game: " + game.id )
        for ( userAnswerHistory <- registredPlayersHistory.values ) {
          val key = GameUserKey(game.id, userAnswerHistory.user.id)
          gameUserHistoryService.callback(StoreData(GameUserHistory( key, userAnswerHistory.answersHistory )) ){
            case DataStored( Right( historyStored ) ) => log.info( key + " history saved")
            case _ => log.error("Unable to store GameUserHistory key " + key )
          }
        }
        gameState = EndGame

        tweetEndGame()

        scorer.save( game.id )

      case _ =>
        log.error("Game "+game.id+" already ended" )
    }
  }


  private def tweetEndGame(){
    Actor.spawn{
      val properties = new Properties()
      properties.load( getClass.getResourceAsStream( "/configuration.properties" ) )
      log.info("Notre application supporte "+registredPlayersHistory.size+" joueurs #challengeUSI2011")
      if( properties.getProperty("endgame.twitter.enabled").toBoolean )
        Twitter.update("Notre application supporte "+registredPlayersHistory.size+" joueurs #challengeUSI2011")
    }
  }

  /**
   * Handle a timeout:
   * - after the login time (not all user requested question 1)
   * - after a synchro time (between questions)
   * - after a question (time elapsed for sending answer)
   */
  private def timeout(timeoutType: TimeoutType.Value) {
    if (timeoutType == TimeoutType.QUESTION) {
      for ( i <- 0 until currentQuestionPlayer.playerIndex ) {
        val userId = currentQuestionPlayer.players( i )
        if (!currentQuestionPlayer.playerActors.contains( userId ) ) {
          val userAnswerHistory = registredPlayersHistory( userId )

          if( userAnswerHistory.answersHistory.find( _.questionIndex == currentQuestionIndex ).isEmpty )
            userAnswerHistory.answerBonus = 0
        }
      }

      currentQuestionPlayer = nextQuestionPlayer
      nextQuestionPlayer = new QuestionPlayer
      currentQuestionIndex += 1
      timer.schedule( TimeoutMessage(TimeoutType.SYNCRO, currentQuestionIndex, game.synchroTimeSec), this )
    }
    else {
      if(timeoutType == TimeoutType.LOGIN) {
        gameState = InGame
      }
      // LOGIN OR SYNCHRO
      //if last question, does nothing and just hope that score and ranking are available ;) <= should be as scorer is synchrone
      if( gameState == InGame ){
        if(currentQuestionIndex == game.questions.size ) endGame()
        else replyQuestion()
      }

    }
  }
}
