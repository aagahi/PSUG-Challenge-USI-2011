package org.psug.usi.rest

import com.sun.jersey.core.util.MultivaluedMapImpl

import akka.dispatch.{Future,Futures}
import org.psug.usi.domain._
import org.psug.usi.service._
import org.psug.usi.store._
import org.specs._
import org.psug.usi.netty.WebServer
import org.psug.usi.Main._

import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner
import com.sun.jersey.api.client._

@RunWith(classOf[JUnitSuiteRunner])
class AdminAuditingSpec extends SpecificationWithJUnit {

  val repositories = new SimpleRepositoryServices
  val webServer : WebServer = new WebServer(listenPort,repositories)
  val listenPort = 12345
      
  val game = Game( questions = 
                     Question( "Q1", Answer( "A11", false )::Answer("A12", true)::Nil, 1 )
                     :: Question( "Q2", Answer( "A21", false )::Answer("A22", true)::Nil, 2 )
                     :: Question( "Q3", Answer( "A31", false )::Answer("A32", true)::Nil, 3 )
                     :: Nil
                 , loginTimeoutSec = 5
                 , synchroTimeSec = 7
                 , questionTimeFrameSec = 11
                 , nbQuestions = 3
                 , flushUserTable = false
                 , nbUsersThreshold = 160 
                 )

  val users = (for( i <- 0 until game.nbUsersThreshold ) yield User( i, "firstname"+i, "lastname"+i, "mail"+i, "password"+i )).toList

  val gameManager = new GameManagerService( repositories.gameUserHistoryService )  
  
  private[this] def webResource( path:String ) = new Client().resource("http://localhost:"+listenPort+path)
  private[this] def queryRanking(key:String, userEmail:String ):ClientResponse = {
    val queryParams = new MultivaluedMapImpl()
    queryParams.add("user_mail", userEmail)
    queryParams.add("authentication_key", key)
    webResource("/api/score").queryParams(queryParams).get(classOf[ClientResponse])
  } 
  
  "Admin auditing score for one user" should {
    doBefore {
      webServer.start
      repositories.start  
      gameManager.go
    }
      
    doAfter {
      gameManager ! StopReceiver
      repositories.stop 
      webServer.stop 
    }
 
    "Succed if the auth key is OK, a game was played, and the queried user played that game" in {
      playGame(gameManager,game,users)      
      val response = queryRanking(WEB_AUTHICATION_KEY, "email0")
      response.getStatus must be_==(ClientResponse.Status.OK.getStatusCode)
      //TODO: check result
    }

    "fail on POST" in {
      
    }
      
    "fail if the authentication key is not given as 'authentication_key' URL parameter" in {
      
    }
      
    "fail if the authentication key is not the good one" in {
      
    }
    

    "fail if the authentication key is OK but no user mail is provided" in {
      
    }
      
    "fail if the authentication key is OK but the provided user is not reigstered" in {
      
    }
      
    "fail if the authentication key is OK but the no game was finished" in {
      
    }
      
    "fail if the authentication key is OK, a finished game exists, but the user didn't played it" in {
      
    }
   
  }
    
    
  ////////////////// Utility //////////////////
    
  //play a game
  private[this] def playGame(gameManager:GameManagerService, game:Game, users:List[User]) : Unit = {
      gameManager !? InitGame(game)


      // Register
      users.map( user => gameManager.remote ! Register( user ) )

     try {
     //ask/answer question
      (0 until game.questions.size) foreach { currentQuestion =>
        println("question " + currentQuestion)
        // Ask for Question i
        val futures = users.map( user => (gameManager.remote !! QueryQuestion( user.id, currentQuestion )).asInstanceOf[Future[QuestionResponse]] )
        Futures.awaitAll( futures )
        futures.map( _.result ).foreach{
          case Some( QuestionResponse( nextQuestion ) )=>
            nextQuestion must be_==( game.questions(currentQuestion ) )
          case _ => fail
        }
  
        // Answser Question i
        users.foreach{
          user =>
            val UserAnswerResponse( answerStatus, score ) = (gameManager.remote !? UserAnswer( user.id, currentQuestion, user.id%(game.questions(currentQuestion).answers.size) ) ).asInstanceOf[UserAnswerResponse]
            val expectedPrevScoreWithBonus = if(currentQuestion < 1) 0 else {
              if( game.questions(currentQuestion).answers( user.id%(game.questions(currentQuestion-1).answers.size) ).status  ) game.questions(currentQuestion-1).value+1 else 0
            }
            val expectedScore = if( answerStatus ) game.questions(currentQuestion).value+expectedPrevScoreWithBonus else expectedPrevScoreWithBonus
            score must be_== ( expectedScore )
        }
      }
     } catch {
       case e:Exception => 
         println(e.getMessage)
         //TODO: why does it not appear in tests ?
         fail(e.getMessage)
         throw e
     }
      // Get userScore slices
      users.foreach{
        user =>
          (gameManager.remote !? QueryScoreSlice( user.id ) ) match {
            case ScoreSlice(scoreSlice) => 
              //TODO check !
            case x => fail("Received unexpected answer: " + x)
          }
      }
      
      // Check history
      users.foreach{
        user =>
        val DataPulled( Some( userHistory ) ) = repositories.gameUserHistoryService !? PullData( GameUserKey( game.id, user.id ) )
        val expectedHistory = game.questions.zipWithIndex.reverse.map{ case( q, i ) => AnswerHistory( i, user.id%(q.answers.size) ) }
        userHistory.asInstanceOf[GameUserHistory].anwsers must be_==( expectedHistory )
      }
  }    
}