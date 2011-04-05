package org.psug.usi.utils

import akka.dispatch.{Futures, Future}
import org.psug.usi.service._
import org.psug.usi.domain.{Question, User, Game}
import util.Random
import math._
import akka.util.Logging

/**
 * User: alag
 * Date: 4/5/11
 * Time: 4:57 PM
 */

class GamePlayer( gameManagerService:GameManagerService, game:Game, users:List[User] ) extends Logging {
  val seed = new Random().nextLong

  def expectedScore( user:User ) = {
    var score = 0
    var bonus = 0
    for( questionIndex <- 0 until game.nbQuestions ){
      val answerIndex  = answer( user, questionIndex )
      val question = game.questions(questionIndex)
      if( question.answers(answerIndex).status ){
        score += question.value + bonus
        bonus += 1
      }
      else {
        bonus = 0
      }
    }
    score
  }




  def answer( user:User, questionIndex:Int ) = {
    val question = game.questions(questionIndex)

    val random = new Random( seed + user.id + questionIndex )
    abs( random.nextInt ) %(question.answers.size)
  }

  def play() {
    gameManagerService !? InitGame( game )

    var currentQuestion = 0

    // Register
    users.map( user => gameManagerService ! Register( user ) )

    for( questionIndex <- 0 until game.nbQuestions ){
      val futures = users.map( user => (gameManagerService !! QueryQuestion( user.id, questionIndex )).asInstanceOf[Future[QuestionResponse]] )
      Futures.awaitAll( futures )

      // Answsers
      users.foreach{
        user =>
          val answerIndex = answer( user, questionIndex )
          val answerIndex2 = answer( user, questionIndex )
          assert( answerIndex == answerIndex2 )
          val UserAnswerResponse( answerStatus, score ) = (gameManagerService !? UserAnswer( user.id, questionIndex, answerIndex ) ).asInstanceOf[UserAnswerResponse]

          if( questionIndex == game.nbQuestions - 1 ){
            assert( score == expectedScore( user ) )
          }


      }
    }


    val GameManagerStats( registredPlayer, currentQuestionPlayersCount, state ) = gameManagerService !? QueryStats
    assert( state == EndGame )
  }

}