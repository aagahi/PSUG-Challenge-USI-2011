package org.psug.usi.domain

/**
 * User: alag
 * Date: 2/17/11
 * Time: 12:19 AM
 */

import org.specs._
import org.psug.usi.store._
import org.psug.usi.service._



import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner
import scala.io.Source

@RunWith(classOf[JUnitSuiteRunner])
class GamesSpec extends SpecificationWithJUnit {



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
    var repositories:SimpleRepositoryServices = null

    setSequential()

    def startRepository:Unit = {
      repositories = new SimpleRepositoryServices
      repositories.start
      repositories.gameRepositoryService.remote !? ClearRepository

    }

    def exitRepository = {
      repositories.gameRepositoryService.remote !? ClearRepository
      repositories.stop
    }

    startRepository.before
    exitRepository.after

    val game = Game( questions = Question( "Q1", Answer( "A1", false )::Answer("A2", false)::Nil, 1 ) :: Nil, nbQuestions = 1 )

    "assign unique id to user when registering" in {
      val DataStored( Right( gameStored ) ) = repositories.gameRepositoryService.remote !? StoreData(game)
      gameStored.asInstanceOf[Game].id must be_!=( game.id )

    }

    "lookup game by id" in {

      val DataStored( Right( gameStored ) ) = repositories.gameRepositoryService.remote !? StoreData(game)
      val DataPulled( Some( gameFound ) ) = repositories.gameRepositoryService.remote !? PullData(gameStored.asInstanceOf[Game].id)
      gameFound.asInstanceOf[Game].questions.head.question must be_==( game.questions.head.question )

    }
  }

}

