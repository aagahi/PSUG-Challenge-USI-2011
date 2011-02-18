package org.psug.usi.games

import org.psug.usi.users.User
import actors.Actor
import org.psug.usi.score.{UserScore, UserResponseAgent, Scorer}

/**
 * User: alag
 * Date: 2/17/11
 * Time: 11:32 PM
 */


case class UserAnswer( userId:Int, questionIndex:Int, answerIndex:Int )
case class UserQuestion( userId:Int, question:Option[Question], scoreSlice:Option[Array[UserScore]] = None )


object GameManagerTimer extends Actor {
  case class QuestionTimeout(questionIndex:Int, timoutSec:Int)

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
class GameManager( val game:Game, val endpoint:Actor ) extends Actor {

  start
  val scorer = new Scorer(game.numPlayer)(10)

  val players = new Array[Int]( game.numPlayer )
  var playerIndex = 0

  var currentQuestionIndex = 0


  def act {
    loop {
      react {
        case user:User => register( user )
        case UserAnswer( userId, questionIndex, answerIndex ) if( questionIndex == currentQuestionIndex ) => answer( userId, answerIndex )
        case GameManagerTimer.QuestionTimeout( questionIndex, timeoutSec ) if( questionIndex == currentQuestionIndex ) => proceedToNextQuestion()
        case _ =>
      }
    }
  }


  /*
   * Long polling stage -> all user register until the required game player is reach, proceedToNextQuestion is called
   */
  def register( user:User ) { appendUserId( user.id ) }

  /*
   * A user answer the current question, when all player respond or timeout occurs proceedToNextQuestion is called
   */
  def answer( userId:Int, answerIndex:Int ){
    val userResponse = new UserResponseAgent( userId, scorer )
    val currentQuestion = game.questions(currentQuestionIndex)

    if( currentQuestion.answers( answerIndex ).status ) userResponse.ok
    else userResponse.ko
 
    players(playerIndex) = userId
    playerIndex += 1
    if( playerIndex >= game.numPlayer ) proceedToNextQuestion()
  }

  private def appendUserId( userId:Int ){
    players(playerIndex) = userId
    playerIndex += 1
    if( playerIndex >= game.numPlayer ) proceedToNextQuestion()
  }

  private def proceedToNextQuestion(){
    for( i <- 0 until playerIndex ){
      val userId = players(i)
      val nextQuestion = if( currentQuestionIndex < game.questions.size ) Some( game.questions(currentQuestionIndex) ) else None
      val scoreSlice = if( currentQuestionIndex > 0 ) Some( scorer.score( userId ) ) else None
      endpoint ! UserQuestion( userId, nextQuestion, scoreSlice )
    }
    GameManagerTimer ! GameManagerTimer.QuestionTimeout( currentQuestionIndex, game.timeoutSec )

    currentQuestionIndex += 1
    playerIndex = 0
  }


}