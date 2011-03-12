package org.psug.usi.domain

/**
 * User: alag
 * Date: 2/17/11
 * Time: 12:19 AM
 */

import org.specs._
import org.psug.usi.service.SimpleRepositoryServices._
import org.psug.usi.store._
import actors.Futures
import scala.io.Source
import org.psug.usi.service._

class GamesSpec extends SpecificationWithJUnit {

  def clearRepository = gameRepositoryService.remoteRef ! ClearRepository

  "a game" should {
    "be definable using xml string" in {
      val game = Game( Source.fromFile( "./test-data/simplegamesession.xml" ).mkString )
      game.loginTimeoutSec must be_==( 3 )
      game.synchroTimeSec must be_==( 5 )
      game.questionTimeFrameSec must be_==( 11 )
      game.nbQuestions must be_==( 6 )
      game.nbUsersThreshold must be_==( 7 )
      game.flushUserTable must be_==( true )

      game.questions.size must be_==( 6 )
      0 to 4 foreach{ i => game.questions(i).value must be_==( 1 ) }
      game.questions( 5 ).value must be_==( 5 )

      game.questions.zipWithIndex.foreach{
        case( question, questionIndex ) =>
        question.question must be_==( "Q"+(questionIndex+1) )
        question.answers.zipWithIndex.foreach{
          case( answer, answserIndex ) =>
          answer.anwser must be_==( "A"+(questionIndex+1)+(answserIndex+1) )
          answer.status must be_==( questionIndex % 4 == answserIndex )  
        }
      }


    }
  }

  "in-memory game repository" should {
    clearRepository.before
     
    val game = Game( questions = Question( "Q1", Answer( "A1", false )::Answer("A2", false)::Nil, 1 ) :: Nil, nbQuestions = 1 )

    "assign unique id to user when registering" in {
      val DataStored( Right( gameStored ) ) = gameRepositoryService.remoteRef !? StoreData(game)
      gameStored.asInstanceOf[Game].id must be_!=( game.id )

    }

    "lookup game by id" in {

      val DataStored( Right( gameStored ) ) = gameRepositoryService.remoteRef !? StoreData(game)
      val DataPulled( Some( gameFound ) ) = gameRepositoryService.remoteRef !? PullData(gameStored.asInstanceOf[Game].id)
      gameFound.asInstanceOf[Game].questions.head.question must be_==( game.questions.head.question )

    }
  }

  "game manager" should {
    clearRepository.before

    val game = Game( questions = Question( "Q1", Answer( "A11", false )::Answer("A12", true)::Nil, 1 )
                    :: Question( "Q2", Answer( "A21", false )::Answer("A22", true)::Nil, 2 )
                    :: Question( "Q3", Answer( "A31", false )::Answer("A32", true)::Nil, 3 )
                    :: Nil,
                    loginTimeoutSec = 10,
                    synchroTimeSec = 10,
                    questionTimeFrameSec = 10,
                    nbQuestions = 3,
                    flushUserTable = false,
                    nbUsersThreshold = 100 )

    val users = for( i <- 0 until game.nbUsersThreshold ) yield User( i, "firstName"+i, "lastName"+i, "email"+i, "password"+i )


    "register all players, ask&get question, score each answer, save user history after last response, and get score slice (no timeout scenario)" in {


      val gameManager = new GameManagerService( game, gameUserHistoryService )

      var currentQuestion = 0

      // Register
      users.map( user => gameManager.remoteRef ! Register( user.id ) )


      // Ask for Q1
      val futuresQ1 = users.map( user => gameManager.remoteRef !! QueryQuestion( user.id, currentQuestion ) )
      Futures.awaitAll( 10000, futuresQ1.toSeq:_* ).foreach{
        case Some( QuestionResponse( nextQuestion ) )=>
          nextQuestion must be_==( game.questions(currentQuestion ) )
        case _ => fail
      }

      // Answser Q1
      users.foreach{
        user =>
          val UserAnswerResponse( answerStatus, score ) = (gameManager.remoteRef !? UserAnswer( user.id, currentQuestion, user.id%(game.questions(currentQuestion).answers.size) ) ).asInstanceOf[UserAnswerResponse]
          val expectedScore = if( answerStatus ) game.questions(currentQuestion).value else 0
          score must be_== ( expectedScore )
      }


      currentQuestion += 1
      // Ask for Q2
      val futuresQ2 = users.map( user => gameManager.remoteRef !! QueryQuestion( user.id, currentQuestion ) )
      Futures.awaitAll( 10000, futuresQ2.toSeq:_* ).foreach{
        case Some( QuestionResponse( nextQuestion ) )=>
          nextQuestion must be_==( game.questions( currentQuestion ) )
        case _ => fail
      }

      // Answser Q2
      users.foreach{
        user =>
          val UserAnswerResponse( answerStatus, score ) = (gameManager.remoteRef !? UserAnswer( user.id, currentQuestion, user.id%(game.questions(currentQuestion).answers.size) ) ).asInstanceOf[UserAnswerResponse]

          val expectedPrevScoreWithBonus = if( game.questions(currentQuestion).answers( user.id%(game.questions(currentQuestion-1).answers.size) ).status  ) game.questions(currentQuestion-1).value+1 else 0
          val expectedScore = if( answerStatus ) game.questions(currentQuestion).value+expectedPrevScoreWithBonus else expectedPrevScoreWithBonus
          score must be_== ( expectedScore )
      }

      currentQuestion += 1
      // Ask for Q3
      val futuresQ3 = users.map( user => gameManager.remoteRef !! QueryQuestion( user.id, currentQuestion ) )
      Futures.awaitAll( 10000, futuresQ3.toSeq:_* ).foreach{
        case Some( QuestionResponse( nextQuestion ) )=>
          nextQuestion must be_==( game.questions( currentQuestion ) )
        case _ => fail
      }

      // Answser Q3
      users.foreach{
        user =>
          val UserAnswerResponse( answerStatus, score ) = (gameManager.remoteRef !? UserAnswer( user.id, currentQuestion, user.id%(game.questions(currentQuestion).answers.size) ) ).asInstanceOf[UserAnswerResponse]
          val expectedScore = if( answerStatus ) (1+1 +2+1 +3+1) else 0
          score must be_== ( expectedScore )
      }

      // Get score slices
      users.foreach{
        user =>
          val scoreSlice = (gameManager.remoteRef !? QueryScoreSlice( user.id ) ).asInstanceOf[Array[UserScore]]
          val minSliceSize = math.min( math.abs( gameManager.scorer.sliceRange.head ), gameManager.scorer.sliceRange.last )
          scoreSlice.size must be_>=( minSliceSize )
          scoreSlice.size must be_<( gameManager.scorer.sliceRange.size )
      }

      // Check history
      users.foreach{
        user =>
        val DataPulled( Some( userHistory ) ) = gameUserHistoryService !? PullData( GameUserKey( game.id, user.id ) )
        val expectedHistory = game.questions.zipWithIndex.reverse.map{ case( q, i ) => AnswerHistory( i, user.id%(q.answers.size) ) }
        userHistory.asInstanceOf[GameUserHistory].anwsers must be_==( expectedHistory )
      }


    }

  }
  
}

