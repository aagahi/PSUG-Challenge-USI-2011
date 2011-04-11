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
import org.jboss.netty.handler.codec.http.HttpResponseStatus

@RunWith(classOf[JUnitSuiteRunner])
class AdminAuditingSpec extends SpecificationWithJUnit {

  implicit val formats = Serialization.formats(NoTypeHints)

  val serverServices = new ServerServices
  val services = new ClientServices()
  import services._

  val webAuthenticationKey = "dummy"
  val listenPort = 12345
  val webServer : WebServer = new WebServer( listenPort, services, webAuthenticationKey )

  val param_key = "authentication_key"
  val param_mail = "user_mail"

  setSequential()

  private[this] def webResource( path:String ) = new Client().resource("http://localhost:"+listenPort+path)

  private[this] def getScore(key:String, userEmail:String ):String = {
    val queryParams = new MultivaluedMapImpl()
    queryParams.add(param_mail, userEmail)
    queryParams.add(param_key, key)
    webResource("/api/score").queryParams(queryParams).get(classOf[String])
  } 
  
  private[this] def getAudit(key:String, userEmail:String, questionIndex:Option[Int] ):String = {
    val queryParams = new MultivaluedMapImpl()
    queryParams.add(param_mail, userEmail)
    queryParams.add(param_key, key)
    val uri = "/api/audit" + (questionIndex match {
      case Some(questionIndex) => "/"+questionIndex
      case None => ""
    })
    webResource( uri ).queryParams(queryParams).get(classOf[String])

  }
  
  //utility method to test both score and audit with bad request parameters
  private[this] def testWithParam(uri:String, params:Map[String,String]) : ClientResponse = {
    val queryParams = new MultivaluedMapImpl()
    params foreach { case(key,value) => queryParams.add(key, value) }
    webResource( uri ).queryParams(queryParams).get(classOf[ClientResponse])
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

    //test for a POST in the uri
    def notGetRequest(uri:String) : Unit = {
      val queryParams = new MultivaluedMapImpl()
      queryParams.add(param_mail, "user@mail.com")
      queryParams.add(param_key, webAuthenticationKey)
      val answer = webResource( uri ).`type`("application/x-www-form-urlencoded").post(classOf[ClientResponse], queryParams) 
          
      answer.getStatus must be_==( HttpResponseStatus.BAD_REQUEST.getCode )
    }
    
    "fail on POST for /api/score" in {
      notGetRequest("/api/score")
    }
      
    "fail on POST for /api/audit" in {
      notGetRequest("/api/audit")
    }

    
    
    //test for bad param name for key
    def badNameForParamKey(uri:String) : Unit = {
      testWithParam(uri, Map(param_key + "bad" -> webAuthenticationKey)).
        getStatus must be_==( HttpResponseStatus.UNAUTHORIZED.getCode ) 
    }
      
    "fail for /api/score if the authentication key is not given as 'authentication_key' URL parameter" in {
      badNameForParamKey("/api/score")
    }

    "fail for /api/audit if the authentication key is not given as 'authentication_key' URL parameter" in {
      badNameForParamKey("/api/audit")
    }
     

    //test for bad param name for key
    def badParamKey(uri:String) : Unit = {
      testWithParam(uri, Map(param_key -> ("bad" + webAuthenticationKey))).
        getStatus must be_==( HttpResponseStatus.UNAUTHORIZED.getCode ) 
    }
    
    "fail for /api/score if the authentication key is not the good one" in {
      badParamKey("/api/score")
    }
    
    "fail for /api/audit if the authentication key is not the good one" in {
      badParamKey("/api/audit")
    }
    

    
    //test for bad param name for key
    def noUserParam(uri:String) : Unit = {
      testWithParam(uri, Map(param_key -> webAuthenticationKey)).
        getStatus must be_==( HttpResponseStatus.BAD_REQUEST.getCode )
    }
    
    "fail for /api/audit if the authentication key is OK but no user mail is provided" in {
      noUserParam("/api/audit")
    }
      
    "fail for /api/score if the authentication key is OK but no user mail is provided" in {
      noUserParam("/api/score")
    }
    
    "fail if the authentication key is OK but no game was finished" in {
      val game = GameGenerator( 3, 4, 160 )
      val users = UserGenerator( userRepositoryService, 160 )
      val gamePlayer = new GamePlayer( gameManagerService, game, users )

      
      testWithParam("/api/score", Map( 
          param_key -> webAuthenticationKey , 
          param_mail -> "mail10" 
       ) ).getStatus must be_==( HttpResponseStatus.BAD_REQUEST.getCode )
       
      testWithParam("/api/audit/10", Map( 
          param_key -> webAuthenticationKey , 
          param_mail -> "mail10" 
       ) ).getStatus must be_==( HttpResponseStatus.BAD_REQUEST.getCode )
    
    }
      
    "fail if the authentication key is OK, a finished game exists, but the user didn't played it" in {
      val game = GameGenerator( 3, 4, 160 )
      val users = UserGenerator( userRepositoryService, 160 )
      val gamePlayer = new GamePlayer( gameManagerService, game, users )

      gamePlayer.play()

      testWithParam("/api/audit/10", Map( 
          param_key -> webAuthenticationKey , 
          param_mail -> "aNonExistingMail@mail.com" 
       ) ).getStatus must be_==( HttpResponseStatus.BAD_REQUEST.getCode )
      
      testWithParam("/api/score", Map( 
          param_key -> webAuthenticationKey , 
          param_mail -> "aNonExistingMail@mail.com" 
       ) ).getStatus must be_==( HttpResponseStatus.BAD_REQUEST.getCode )
    }
   
  }
  
  
}


object AdminAuditingSpecMain {
  def main(args: Array[String]) { new AdminAuditingSpec().main(args) }
}


