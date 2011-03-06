package org.psug.usi.service

import actors.{OutputChannel, Actor}
import collection.mutable.HashMap
import org.psug.usi.domain._

/**
 * User: alag
 * Date: 2/17/11
 * Time: 11:32 PM
 */

case class Register( userId:Int )

case class UserAnswer( userId:Int, questionIndex:Int, answerIndex:Int )

// Question send to the user => if we assume that we send this question to an actor that has a ref on user id, we should not need to have userId in this class
case class UserQuestion( userId:Int, question:Option[Question], scoreSlice:Option[Array[UserScore]] = None )



object GameManagerTimer {
  case class QuestionTimeout(questionIndex:Int, timoutSec:Int)
}
class GameManagerTimer extends Actor {
  import GameManagerTimer._

  start
  def act {
    loop {
      react {
        case questionTimeout:QuestionTimeout =>
          Thread.sleep(questionTimeout.timoutSec*1000)
          sender ! questionTimeout
      }
    }
  }
}

/**
 * A game manager: handle question/anwser and timeout
 */
class GameManagerService( val game:Game ) extends RemoteService {
  import GameManagerTimer._


  val scorer = new Scorer(game.numPlayer)
  val timer = new GameManagerTimer

  val players = new Array[Int]( game.numPlayer )
  var playerIndex = 0

  val playerActors = new HashMap[Int,OutputChannel[Any]]

  var currentQuestionIndex = -1 // start -1 cause proceedToNextQuestion do a +=1 -> 1st question case


  def act {
    loop {
      react {
        case Register( userId ) => register( userId )
        case UserAnswer( userId, questionIndex, answerIndex ) if( questionIndex == currentQuestionIndex ) => answer( userId, answerIndex )
        case QuestionTimeout( questionIndex, timeoutSec ) if( questionIndex == currentQuestionIndex ) => timeout()
        case x => 
      }
    }
  }


  /*
   * Long polling stage -> all user register until the required game player is reach, proceedToNextQuestion is called
   */
  private def register( userId:Int ){
    players(playerIndex) = userId
    playerIndex += 1
    playerActors( userId ) =  sender
    if( playerIndex >= game.numPlayer ) proceedToNextQuestion()
  }

  /*
   * A user answer the current question, when all player respond or timeout occurs proceedToNextQuestion is called
   */
  def answer( userId:Int, answerIndex:Int ){
    val currentQuestion = game.questions(currentQuestionIndex)

    val answerValue = if( currentQuestion.answers( answerIndex ).status ) currentQuestion.value else 0
    scorer ! ScorerAnwserValue( userId, answerValue )

    playerActors( userId ) = sender
    if( playerActors.size >= game.numPlayer ){
      proceedToNextQuestion()
    }

  }

  private def proceedToNextQuestion(){
    currentQuestionIndex += 1

    playerActors.foreach{
      case ( userId, playerActor ) =>
        val nextQuestion = if( currentQuestionIndex < game.questions.size ) Some( game.questions(currentQuestionIndex) ) else None
        val scoreSlice = if( currentQuestionIndex > 0 ) Some( scorer.scoreSlice( userId ) ) else None
        playerActor ! UserQuestion( userId, nextQuestion, scoreSlice )
    }
    playerActors.clear
    
    timer ! QuestionTimeout( currentQuestionIndex, game.timeoutSec )


  }


  private def timeout(){
    for( userId <- players ){
      if( !playerActors.contains( userId ) ){
        scorer ! ScorerAnwserValue( userId, 0 )
      }
    }
    proceedToNextQuestion()
  }
}