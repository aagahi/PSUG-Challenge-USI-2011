package org.psug.usi.rest

/**
 * User: alag
 * Date: 4/7/11
 * Time: 10:31 AM
 */

import org.psug.usi.domain._
import org.psug.usi.service._
import org.psug.usi.store._
import org.specs._
import org.psug.usi.netty.WebServer
import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner
import com.sun.jersey.api.client._
import net.liftweb.json.NoTypeHints
import net.liftweb.json.Serialization
import org.jboss.netty.handler.codec.http.CookieEncoder
import akka.dispatch.Futures
import org.psug.usi.utils.{AsyncExecutor, GameGenerator, UserGenerator}


@RunWith(classOf[JUnitSuiteRunner])
class GamePlaySpec extends SpecificationWithJUnit {

  implicit val formats = Serialization.formats(NoTypeHints)

  val serverServices = new SimpleRepositoryServices
  val services = new ClientServices()
  import services._


def startRepository:Unit = {
    serverServices.launch
    webServer.start
    userRepositoryService !? ClearRepository
    gameRepositoryService !? ClearRepository
  }

  def exitRepository = {
    userRepositoryService !? ClearRepository
    gameRepositoryService !? ClearRepository
    webServer.stop
    serverServices.shutdown
  }




  val webAuthenticationKey = "dummy"
  val listenPort = 12345
  val webServer : WebServer = new WebServer( listenPort, services, webAuthenticationKey )

  private[this] def webResource( path:String ) = new Client().resource("http://localhost:"+listenPort+path)

  // query /api/question/N
  def queryQuestionN( user:User, questionIndex:Int ):String = {
    val cookieEncoder = new CookieEncoder( false )
    cookieEncoder.addCookie("session_key",AuthenticationToken( user.id, user.mail ))
    webResource("/api/question/"+questionIndex).header("Set-Cookie", cookieEncoder.encode() ).get(classOf[String] )
  }





  "registred player" should {

    startRepository.before
    exitRepository.after

    "query question 0 should return error" in {
    }

    "query question 1" in {
      val game = GameGenerator( 3, 4, 16 )
      val users = UserGenerator( userRepositoryService, 16 )
      gameManagerService !? InitGame(game)
      users.foreach( user => gameManagerService ! Register( user ) )

      val currentQuestion = 0
      val futures = users.map{
        user => AsyncExecutor().execute{ queryQuestionN( user, currentQuestion+1 ) }
      }

      Futures.awaitAll( futures )
      futures.map( _.result ).foreach{
        case Some( str )=>
          val question = Serialization.read[Question]( str )
          question must be_==( game.questions( currentQuestion ) )
        case _ => fail
      }

      

    }
  }


} 