package org.psug.usi.service

import actors.{OutputChannel, Actor}
import org.psug.usi.domain._
import org.psug.usi.store.StoreData
import collection.mutable.HashMap

/**
 * User: alag
 * Date: 2/17/11
 * Time: 11:32 PM
 */

case class InitGame(game: Game)

case class Register(user: User)

case object QueryStats

case class GameManagerStats(registredPlayer: Int, currentQuestionPlayersCount: Int)

case class QueryQuestion(userId: Int, questionIndex: Int)

case class UserAnswer(userId: Int, questionIndex: Int, answerIndex: Int)

case class QueryScoreSlice(userId: Int)

// Question send to the user => if we assume that we send this question to an actor that has a ref on user id, we should not need to have userId in this class
case class UserAnswerResponse(answerStatus: Boolean, score: Int)

case class QuestionResponse(question: Question)

object TimeoutType extends Enumeration {
  val LOGIN, SYNCRO, QUESTION = Value
}

case class TimeoutMessage(timeoutType: TimeoutType.Value, questionIndex: Int, timoutSec: Int)

trait GameManagerTimer extends Actor {
  start

  def act {
    loop {
      react {
        case questionTimeout: TimeoutMessage => handleQuestionTimeout(questionTimeout)
        case x => handleOtherMessage(x)
      }
    }
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
 * A game manager: handle question/anwser and timeout
 */
class GameManagerService(val gameUserHistoryRepositoryService: GameUserHistoryRepositoryService,
                         var timer: GameManagerTimer = new DefaultGameManagerTimer)
  extends DefaultServiceConfiguration with Service with RemoteService {

  override lazy val symbol = 'GameManagerService


  var game: Game = _
  var scorer:Scorer = _

  class QuestionPlayer {
    var playerIndex = 0
    val players = new Array[Int](game.nbUsersThreshold)
    val playerActors = new HashMap[Int, OutputChannel[Any]]
  }


  var currentQuestionIndex = 0

  var currentQuestionPlayer : QuestionPlayer = null
  var nextQuestionPlayer : QuestionPlayer = null

  val registredPlayersHistory = new HashMap[Int, UserAnswerHistory]

  def act {
    loop {
      react {
        case InitGame(game) => initGame(game)
        case Register(user) => register(user)
        case QueryStats => sender ! GameManagerStats(registredPlayersHistory.size, currentQuestionPlayer.playerIndex)
        case QueryQuestion(userId, questionIndex) => queryQuestion(userId, questionIndex)
        case UserAnswer(userId, questionIndex, answerIndex) if (questionIndex == currentQuestionIndex) => answer(userId, answerIndex)
        case QueryScoreSlice(userId) => queryScoreSlice(userId)
        case TimeoutMessage(timeoutType, questionIndex, timeoutSec) if (questionIndex == currentQuestionIndex) => timeout(timeoutType)
        case Exit => println("service " + symbol + " exiting"); exit()
        case x =>
      }
    }
  }

  private def initGame(game: Game): Unit = {
    this.game = game
    scorer = new Scorer(game.nbUsersThreshold)
    currentQuestionIndex = 0
    registredPlayersHistory.clear()
    currentQuestionPlayer = new QuestionPlayer
    nextQuestionPlayer = new QuestionPlayer
  }

  private def register(user:User) {

    if (registredPlayersHistory.size == 0) {
      // initialize the logintimeout timer for long polling
      timer ! TimeoutMessage(TimeoutType.LOGIN, currentQuestionIndex, game.loginTimeoutSec)
    }
    if (registredPlayersHistory.size < game.nbUsersThreshold) {
      registredPlayersHistory(user.id) = UserAnswerHistory( user, 0, Nil )
    }
  }

  /*
  * A user query a question
  */
  private def queryQuestion(userId: Int, questionIndex: Int) {
    val questionPlayer = if (questionIndex > currentQuestionIndex) nextQuestionPlayer else currentQuestionPlayer

    questionPlayer.players(questionPlayer.playerIndex) = userId
    questionPlayer.playerIndex += 1
    questionPlayer.playerActors(userId) = sender

    if (nextQuestionPlayer.playerIndex >= registredPlayersHistory.size) {
      currentQuestionPlayer = nextQuestionPlayer
      nextQuestionPlayer = new QuestionPlayer
      currentQuestionIndex += 1
      replyQuestion()
    }
    else if (questionIndex == currentQuestionIndex && currentQuestionPlayer.playerIndex >= registredPlayersHistory.size) {
      replyQuestion()
    }
  }

  private def answer(userId: Int, answerIndex: Int) {

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
    
    if (currentQuestionIndex == game.nbQuestions - 1) {
      gameUserHistoryRepositoryService ! StoreData(GameUserHistory(GameUserKey(game.id, userId), userAnswerHistory.answersHistory ))
    }

  }

  private def queryScoreSlice(userId: Int) {
    sender ! scorer.scoreSlice( registredPlayersHistory(userId).user )
  }

  private def replyQuestion() {

    currentQuestionPlayer.playerActors.foreach {
      case (userId, playerActor) =>
        playerActor ! QuestionResponse(game.questions(currentQuestionIndex))
    }
    currentQuestionPlayer.playerActors.clear
    timer ! TimeoutMessage(TimeoutType.QUESTION, currentQuestionIndex, game.questionTimeFrameSec)


  }

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
      // LOGIN OR SYNCHRO
      replyQuestion()
    }
  }
}
