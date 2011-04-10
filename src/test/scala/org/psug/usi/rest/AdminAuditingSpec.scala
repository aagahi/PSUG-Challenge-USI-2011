package org.psug.usi.rest

import com.sun.jersey.core.util.MultivaluedMapImpl

import org.psug.usi.domain._
import org.psug.usi.service._
import org.psug.usi.store._
import org.specs._
import org.psug.usi.netty.WebServer
import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner
import com.sun.jersey.api.client._
import net.liftweb.json.{NoTypeHints, Serialization}
import net.liftweb.json.Serialization.read
import org.psug.usi.utils.{UserGenerator, GameGenerator, GamePlayer}
import scala.util.Random

@RunWith(classOf[JUnitSuiteRunner])
class AdminAuditingSpec extends SpecificationWithJUnit {

  implicit val formats = Serialization.formats(NoTypeHints)

  val serverServices = new SimpleRepositoryServices
  val services = new ClientServices()
  import services._

  val webAuthenticationKey = "dummy"
  val listenPort = 12345
  val webServer : WebServer = new WebServer( listenPort, services, webAuthenticationKey )





  private[this] def webResource( path:String ) = new Client().resource("http://localhost:"+listenPort+path)

  private[this] def getScore(key:String, userEmail:String ):String = {
    val queryParams = new MultivaluedMapImpl()
    queryParams.add("user_mail", userEmail)
    queryParams.add("authentication_key", key)
    webResource("/api/score").queryParams(queryParams).get(classOf[String])
  } 
  private[this] def getAudit(key:String, userEmail:String, questionIndex:Option[Int] ):String = {
    val queryParams = new MultivaluedMapImpl()
    queryParams.add("user_mail", userEmail)
    queryParams.add("authentication_key", key)
    val uri = "/api/audit" + (questionIndex match {
      case Some(questionIndex) => "/"+questionIndex
      case None => ""
    })
    webResource( uri ).queryParams(queryParams).get(classOf[String])

  }

  "admin auditing" should {
    doBefore {
      serverServices.launch
      webServer.start
      userRepositoryService !? ClearRepository
      gameRepositoryService !? ClearRepository
      gameUserHistoryService !? ClearRepository
    }
      
    doAfter {
      gameUserHistoryService !? ClearRepository
      gameRepositoryService !? ClearRepository
      userRepositoryService !? ClearRepository
      webServer.stop
      serverServices.shutdown
    }

    "provide user score if good auth key is provided, a game was played" in {

      val game = GameGenerator( 3, 4, 160 )
      val users = UserGenerator( userRepositoryService, 160 )

      val gamePlayer = new GamePlayer( gameManagerService, game, users )
      gamePlayer.play()

      val user = users(10)

      val ranking = read[RankingVO](getScore( webAuthenticationKey, user.mail))
      val expectedRanking = gamePlayer.expectedScoreSlice(user)
      ranking.deepEquals( expectedRanking ) must beTrue

    }

    "provide user answers if good auth key is provided, a game was played" in {

      val game = GameGenerator( 3, 4, 160 )
      val users = UserGenerator( userRepositoryService, 160 )

      val gamePlayer = new GamePlayer( gameManagerService, game, users )
      gamePlayer.play()

      0 to 16 foreach {
        i =>
        val user = users( Random.nextInt( users.size ))
        val answers = read[AnswersHistoryVO](getAudit( webAuthenticationKey, user.mail, None))
        val expectedAnswersHistory = gamePlayer.expectedAnswersHistoryVO(user)
        expectedAnswersHistory.deepEquals( answers ) must beTrue

        val questionIndex = Random.nextInt( game.questions.size )
        // +1 on current question (we assume it start at 1 on api side)
        val answer = read[AnswerHistoryVO](getAudit( webAuthenticationKey, user.mail, Some(questionIndex+1)))
        val expectedAnswerHistory = gamePlayer.expectedAnswerHistoryVO(user,questionIndex)
        expectedAnswerHistory must be_==( answer )
      }

    }


/*
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
    */
   
  }

}