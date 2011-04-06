package org.psug.usi.utils

import akka.dispatch.{Futures, Future}
import org.psug.usi.service._
import util.Random
import math._
import akka.util.Logging
import org.psug.usi.domain._
import org.psug.usi.utils.RankingUtil._
/**
 * User: alag
 * Date: 4/5/11
 * Time: 4:57 PM
 */

class GamePlayer( gameManagerService:GameManagerService, game:Game, users:List[User] ) extends Logging {
  val seed = new Random().nextLong


  lazy val sortedScores = users.map( user => UserScore(user, expectedScore(user)) ).sorted

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


  def expectedScoreSlice(user: User, sliceRange: Range = -10 to 10, topSize:Int = 100 ): Ranking = {
    val userScore = sortedScores.find( _.user == user ).get

    val indexOfUser: Int = sortedScores.indexOf(userScore)
    val begin = if (indexOfUser + sliceRange.start < 0) 0 else (indexOfUser + sliceRange.start)
    val end = if (sortedScores.size < indexOfUser + sliceRange.end) sortedScores.size else indexOfUser + sliceRange.end
    val before = ListScores(sortedScores.slice(begin, indexOfUser))
    val after = ListScores(sortedScores.slice(indexOfUser + 1, end))
    Ranking(userScore.score, ListScores(sortedScores.take(topSize)) , before, after)
  }



  def answer( user:User, questionIndex:Int ) = {
    val question = game.questions(questionIndex)

    val random = new Random( seed + user.id + questionIndex )
    abs( random.nextInt ) %(question.answers.size)
  }

  def play() {
    gameManagerService !? InitGame( game )


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



    users.foreach{
      user =>
      val ScoreSlice( ranking ) =  gameManagerService !? QueryScoreSlice( user.id )
      val expectedSlice = expectedScoreSlice( user )
      assert( isSorted( expectedSlice ) )
      assert( isSorted( ranking ))

      ranking.top_scores.mail.foreach{
        mail =>
        val countOccurence = ranking.top_scores.mail.foldLeft(0){ (i,s) => if( s == mail ) i+1 else i }
        assert( countOccurence == 1 )

      }

      expectedSlice.top_scores.mail.foreach{
        mail =>
        val countOccurence = ranking.top_scores.mail.foldLeft(0){ (i,s) => if( s == mail ) i+1 else i }
        assert( countOccurence == 1 )

      }

      assert( expectedSlice.score == ranking.score )
      assert( expectedSlice.deepEquals( ranking ) )
      
    }

    
  }

}