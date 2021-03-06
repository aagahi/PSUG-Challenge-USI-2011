package org.psug.usi.rest

import org.psug.usi.domain.{Game, RegisterGame}
import org.psug.usi.store.{PullData, DataPulled, ClearRepository}
import org.specs._

import com.sun.jersey.api.client._

import net.liftweb.json._
import org.psug.usi.netty._
import org.psug.usi.service.ServerServices
import scala.io.Source

import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner
@RunWith(classOf[JUnitSuiteRunner])
class GameRegistrationSpec extends SpecificationWithJUnit {

  implicit val formats = Serialization.formats(NoTypeHints)
  val listenPort = 12345

  val webAuthenticationKey = "dummy"

  def webResource( path:String ) = new Client().resource("http://localhost:"+listenPort+path)

  val setup = new SpecContext {
    val repositories = new ServerServices
    val webServer : WebServer = new WebServer(listenPort,repositories,webAuthenticationKey)

    before{ webServer.start; repositories.launch; repositories.gameRepositoryService !? ClearRepository }
    after{ webServer.stop ; repositories.shutdown }
  }

  setSequential()

  def registerGame(gameRegistration : RegisterGame ):ClientResponse = {
     webResource("/api/game/").header("Content-Type","application/json").post(classOf[ClientResponse], Serialization.write(gameRegistration))
  }


  "game registration" should {
    shareVariables()

    "return forbidden http status if wrong authtoken is provided" in {
      val parameters = Source.fromFile( "./test-data/simplegamesession.xml" ).mkString

      val authToken = ""

      val response = registerGame( RegisterGame( authToken, parameters ) )
      response.getStatus must be_==(ClientResponse.Status.UNAUTHORIZED.getStatusCode)
    }
    "store game if approprite authtoken is provided" in {
      val parameters = Source.fromFile( "./test-data/simplegamesession.xml" ).mkString
      val response = registerGame( RegisterGame( webAuthenticationKey, parameters ) )
      response.getStatus must be_==(ClientResponse.Status.CREATED.getStatusCode)


      val DataPulled( Some( gameFound ) ) = setup.repositories.gameRepositoryService !? PullData(1)
      gameFound must be_==( Game( parameters).copyWithAutoGeneratedId( 1 ) )

    }

  }
}