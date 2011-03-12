package org.psug.usi.service

import actors.{OutputChannel, Actor}
import org.psug.usi.domain._
import org.psug.usi.store.StoreData
import collection.mutable.{ListBuffer, HashMap}

/**
 * User: alag
 * Date: 2/17/11
 * Time: 11:32 PM
 */

case class Register( userId:Int )
case object QueryStats
case class GameManagerStats( registredPlayer:Int, currentQuestionPlayersCount:Int )
case class QueryQuestion( userId:Int, questionIndex:Int )
case class UserAnswer( userId:Int, questionIndex:Int, answerIndex:Int )
case class QueryScoreSlice( userId:Int )


// Question send to the user => if we assume that we send this question to an actor that has a ref on user id, we should not need to have userId in this class
case class UserAnswerResponse( answerStatus:Boolean, score:Int )
case class QuestionResponse( question:Question )


object TimeoutType extends Enumeration { val LOGIN, SYNCRO, QUESTION  = Value }

case class TimeoutMessage( timeoutType:TimeoutType.Value, questionIndex:Int, timoutSec:Int)


trait GameManagerTimer extends Actor {
  start
  def act {
    loop {
      react {
        case questionTimeout:TimeoutMessage => handleQuestionTimeout( questionTimeout )
        case x => handleOtherMessage( x )
      }
    }
  }
  def handleQuestionTimeout( questionTimeout:TimeoutMessage )
  def handleOtherMessage( message:Any ){}
}


class DefaultGameManagerTimer extends GameManagerTimer {
  def handleQuestionTimeout( questionTimeout:TimeoutMessage ){
    Thread.sleep(questionTimeout.timoutSec*1000)
    sender ! questionTimeout
  }
}

/**
 * A game manager: handle question/anwser and timeout
 */
class GameManagerService( val game:Game,
                          val gameUserHistoryRepositoryService:GameUserHistoryRepositoryService,
                          val timer:GameManagerTimer=new DefaultGameManagerTimer )
  extends  DefaultServiceConfiguration with Service with RemoteService{

  override lazy val symbol = 'GameManagerService

  val scorer = new Scorer(game.nbUsersThreshold)

  var currentQuestionIndex = 0
  var registredPlayers = 0

  class QuestionPlayer{
    var playerIndex = 0
    val players = new Array[Int]( game.nbUsersThreshold )
    val playerActors = new HashMap[Int,OutputChannel[Any]]
  }
  var currentQuestionPlayer = new QuestionPlayer
  var nextQuestionPlayer = new QuestionPlayer


  val playersHistory = new HashMap[Int,List[AnswerHistory]]

  def act {
    loop {
      react {
        case Register( userId ) => register( userId )
        case QueryStats => sender ! GameManagerStats( registredPlayers, currentQuestionPlayer.playerIndex )
        case QueryQuestion( userId, questionIndex ) => queryQuestion( userId, questionIndex )
        case UserAnswer( userId, questionIndex, answerIndex ) if( questionIndex == currentQuestionIndex ) => answer( userId, answerIndex )
        case QueryScoreSlice( userId ) => queryScoreSlice( userId )
        case TimeoutMessage( timeoutType, questionIndex, timeoutSec ) if( questionIndex == currentQuestionIndex ) => timeout(timeoutType)
        case Exit => println("service " + symbol +" exiting"); exit()
        case x =>
      }
    }
  }



  private def register( userId:Int ){
    if( registredPlayers == 0 ){
      // initialize the logintimeout timer for long polling
      timer ! TimeoutMessage( TimeoutType.LOGIN, currentQuestionIndex, game.loginTimeoutSec )
    }
    if( registredPlayers < game.nbUsersThreshold ){
      registredPlayers += 1
    }
  }


  /*
   * A user query a question
   */
  private def queryQuestion(  userId:Int, questionIndex:Int ){
    val questionPlayer = if( questionIndex > currentQuestionIndex ) nextQuestionPlayer else currentQuestionPlayer

    questionPlayer.players(questionPlayer.playerIndex) = userId
    questionPlayer.playerIndex += 1
    questionPlayer.playerActors( userId ) =  sender

    if( nextQuestionPlayer.playerIndex >= registredPlayers ){
      currentQuestionPlayer = nextQuestionPlayer
      nextQuestionPlayer = new QuestionPlayer
      currentQuestionIndex += 1
      replyQuestion()
    }
    else if( questionIndex == currentQuestionIndex && currentQuestionPlayer.playerIndex >= registredPlayers ){
      replyQuestion()
    }
  }


  private def answer( userId:Int, answerIndex:Int ){

    playersHistory( userId ) = AnswerHistory( currentQuestionIndex, answerIndex ) :: playersHistory.getOrElse( userId, Nil )

    val currentQuestion = game.questions(currentQuestionIndex)

    val answerValue = if( currentQuestion.answers( answerIndex ).status ) currentQuestion.value else 0
    val userScore = scorer.scoreAnwser( ScorerAnwserValue( userId, answerValue ) )

    sender ! UserAnswerResponse( userScore.bonus > 0, userScore.score )
    if( currentQuestionIndex == game.nbQuestions-1 ){
      gameUserHistoryRepositoryService ! StoreData( GameUserHistory( GameUserKey( game.id, userId ), playersHistory.getOrElse( userId, Nil ) ) )
    }

  }

  private def queryScoreSlice( userId:Int ){
    sender ! scorer.scoreSlice( userId )
  }

  private def replyQuestion(){

    currentQuestionPlayer.playerActors.foreach{
      case ( userId, playerActor ) =>
        playerActor ! QuestionResponse( game.questions(currentQuestionIndex)  )
    }
    currentQuestionPlayer.playerActors.clear
    timer ! TimeoutMessage( TimeoutType.QUESTION, currentQuestionIndex, game.questionTimeFrameSec )


  }


  private def timeout(timeoutType:TimeoutType.Value){

    if( timeoutType == TimeoutType.QUESTION ){
      for( userId <- currentQuestionPlayer.players ){
        if( !currentQuestionPlayer.playerActors.contains( userId ) ){
          scorer.scoreAnwser( ScorerAnwserValue( userId, 0 ) )
        }
      }

      currentQuestionPlayer = nextQuestionPlayer
      nextQuestionPlayer = new QuestionPlayer
      currentQuestionIndex += 1
      timer ! TimeoutMessage( TimeoutType.SYNCRO, currentQuestionIndex, game.synchroTimeSec )

    }
    else{ // LOGIN OR SYNCHRO
      replyQuestion()
    }
  }
}
