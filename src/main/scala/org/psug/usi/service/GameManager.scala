package org.psug.usi.service

import org.psug.usi.domain._
import collection.mutable.HashMap
import akka.actor.Channel
import akka.util.Logging
import org.psug.usi.store.{DataPulled, StoreData}
import akka.dispatch.Future
import org.psug.usi.akka.Receiver

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

case object QueryStats

case class GameManagerStats(registredPlayer: Int, currentQuestionPlayersCount: Int, state:GameState )

case class QueryQuestion(userId: Int, questionIndex: Int)

case class UserAnswer(userId: Int, questionIndex: Int, answerIndex: Int)

case class QueryScoreSlice(userId: Int)
case class QueryScoreSliceAudit( userEmail: String)
//answer to QueryScoreSlice may be "don't ask" if the game is not finished
trait ScoreSliceAnswer
case object ScoreSliceUnavailable extends ScoreSliceAnswer
case class ScoreSlice(r:Ranking) extends ScoreSliceAnswer


// Question send to the user => if we assume that we send this question to an actor that has a ref on user id, we should not need to have userId in this class
case class UserAnswerResponse(answerStatus: Boolean, score: Int)

case class QuestionResponse(question: Question)

object TimeoutType extends Enumeration {
  val LOGIN, SYNCRO, QUESTION = Value
}

case class TimeoutMessage(timeoutType: TimeoutType.Value, questionIndex: Int, timoutSec: Int)

trait GameManagerTimer extends Receiver {
  start

  def receive = {
    case questionTimeout: TimeoutMessage => handleQuestionTimeout(questionTimeout)
    case x => handleOtherMessage(x)
  }

  def handleQuestionTimeout(questionTimeout: TimeoutMessage)

  def handleOtherMessage(message: Any) {}
}

class DefaultGameManagerTimer extends GameManagerTimer {
  def handleQuestionTimeout(questionTimeout: TimeoutMessage) {
    Thread.sleep(questionTimeout.timoutSec * 1000)
    sender ! questionTimeout
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
class GameManager( gameUserHistoryRepositoryService: GameUserHistoryRepositoryService,
                   userRepositoryService:UserRepositoryService,
                   timer: GameManagerTimer = new DefaultGameManagerTimer)
  extends Receiver with Logging {
  


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


  def receive = {
    case InitGame(game) => initGame(game)
    case Register(user) => register(user)
    case QueryStats => 
      sender ! GameManagerStats(registredPlayersHistory.size, currentQuestionPlayer.playerIndex,gameState)
    case QueryQuestion(userId, questionIndex) 
      if(registredPlayersHistory.isDefinedAt(userId)) 
      => queryQuestion(userId, questionIndex)
    case UserAnswer(userId, questionIndex, answerIndex) 
      if (registredPlayersHistory.isDefinedAt(userId) && questionIndex == currentQuestionIndex)
      => answer(userId, answerIndex)
    case TimeoutMessage(timeoutType, questionIndex, timeoutSec) 
      if (questionIndex == currentQuestionIndex) 
      => timeout(timeoutType)
    case QueryScoreSlice(userId) => queryScoreSlice(userId)
    case QueryScoreSliceAudit(userEmail) => queryScoreSliceAudit(userEmail)
    case StopReceiver => stop()
    case x => //TODO : reply an error message ?
  }

  /**
   * Initialize a game if it is not already initialized.
   */
  private def initGame(game: Game): Unit = {
    gameState match {
      case Uninitialized => 
        this.game = game
        scorer = new Scorer(game.nbUsersThreshold)
        currentQuestionIndex = 0
        registredPlayersHistory.clear()
        currentQuestionPlayer = new QuestionPlayer
        nextQuestionPlayer = new QuestionPlayer
        gameState = Initialized
        sender ! InitGameSuccess
      case _ => sender ! ErrorAlreadyStarted
    }
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
       } else { // user is already registered
         //nothing ? 
       }
      } else {
        log.error("TODO: error message: max number of user logged-in for that game")
      }
    }
    
    gameState match {
      case Initialized =>
        // initialize the logintimeout timer for long polling
        timer ! TimeoutMessage(TimeoutType.LOGIN, currentQuestionIndex, game.loginTimeoutSec)
        this.gameState = WaitingRegistrationAndQ1
        //add that user
        tryToAddUser
      case WaitingRegistrationAndQ1 =>
        tryToAddUser
      case _ => 
        log.error("TODO: error message: Error: cannot add user with mail %s. Registration are closed". format(user.mail))
    }
  }

  /**
   * A user query a question. If the question number is the currently processed, 
   * we wait for all user to query that question and then reply to all of them.
   */
  private def queryQuestion(userId: Int, questionIndex: Int) {
    //TODO: why not check that the user is in that game ?
    
    //TODO: I don't see a case in the spec where a user is allowed to
    //query for a question that is not currentQuestionIndex
    val questionPlayer = if (questionIndex > currentQuestionIndex) nextQuestionPlayer else currentQuestionPlayer

    questionPlayer.players(questionPlayer.playerIndex) = userId
    questionPlayer.playerIndex += 1
    questionPlayer.playerActors(userId) = sender

    if (nextQuestionPlayer.playerIndex >= registredPlayersHistory.size ) {
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
    else if (questionIndex == currentQuestionIndex && currentQuestionPlayer.playerIndex >= registredPlayersHistory.size) {
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
        userAnswerHistory.answerBonus = bonus + 1
        currentQuestion.value + bonus
      } else {
        userAnswerHistory.answerBonus = 0
        0
      }

    val userScore = scorer.scoreAnwser(ScorerAnwserValue(userAnswerHistory.user, answerValue))

    sender ! UserAnswerResponse( answerValue > 0, userScore.score)


    if (currentQuestionIndex == game.nbQuestions - 1 && userAnswerCount == registredPlayersHistory.size ) {
      endGame()
    }

  }

  /**
   * Query for the ranking of the given user.
   * Can be done only when EndGame state is reached 
   * (last question answered and SynchroTime expired)
   * 
   * Return Some(List(scores)) if score are available
   * or None if it is not the time to ask
   */
  private def queryScoreSlice(userId: Int) {
    gameState match {
      case EndGame =>
        sender ! ScoreSlice(scorer.scoreSlice( registredPlayersHistory(userId).user ))
      case _ => 
        //TODO: error
        log.error("Error: ask for the score or ranking on a non finished game")
        sender ! ScoreSliceUnavailable
    }
  }

  /**
   * Audit query for the ranking of the given user.
   * Return Some(List(scores)) if score are available
   * or None if it is not the time to ask
   */
  private def queryScoreSliceAudit(userEmail: String) {
    val target = sender
    (userRepositoryService !! PullDataByEmail( userEmail )).asInstanceOf[Future[DataPulled[Int]]].onComplete(
      future => future.result match {
        case Some( DataPulled( Some( data:User ) ) ) => target ! ScoreSlice( scorer.scoreSlice( data ) )
        case _ => log.warn( "Unexpected repo result for user email " + userEmail )
      }
    )

  }



  /**
   * Send the text of the current question to all users who asked for.
   * User who didn't asked for the Question N at that time won't be able
   * to answer it
   * @return
   */
  private def replyQuestion() {
    userAnswerCount = 0
    currentQuestionPlayer.playerActors.foreach {
      case (userId, playerActor) =>
        playerActor ! QuestionResponse(game.questions(currentQuestionIndex))
    }
    currentQuestionPlayer.playerActors.clear
    timer ! TimeoutMessage(TimeoutType.QUESTION, currentQuestionIndex, game.questionTimeFrameSec)


  }

  /**
   * End the game
   * Store all user answer history
   * @return
   */
  private def endGame(){
    gameState match {
      case InGame =>
        for (userId <- currentQuestionPlayer.players) {
          val userAnswerHistory = registredPlayersHistory( userId )
          gameUserHistoryRepositoryService ! StoreData(GameUserHistory(GameUserKey(game.id, userId), userAnswerHistory.answersHistory ))
        }
        gameState = EndGame
      case _ =>
        log.error("TODO: error message: Not supposed to query question in current game state " + gameState )
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
      for (userId <- currentQuestionPlayer.players) {
        if (!currentQuestionPlayer.playerActors.contains( userId ) ) {
          val userAnswerHistory = registredPlayersHistory( userId )
          userAnswerHistory.answerBonus = 0
        }
      }

      currentQuestionPlayer = nextQuestionPlayer
      nextQuestionPlayer = new QuestionPlayer
      currentQuestionIndex += 1
      timer ! TimeoutMessage(TimeoutType.SYNCRO, currentQuestionIndex, game.synchroTimeSec)
    }
    else {
      if(timeoutType == TimeoutType.LOGIN) {
        gameState = InGame
      }
      // LOGIN OR SYNCHRO
      //if last question, does nothing and just hope that score and ranking are available ;) <= should be as scorer is synchrone
      if( gameState == InGame ){
        if(currentQuestionIndex == game.nbQuestions - 1) endGame()
        else replyQuestion()
      }

    }
  }
}
