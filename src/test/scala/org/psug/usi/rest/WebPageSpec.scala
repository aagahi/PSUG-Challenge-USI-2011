package org.psug.usi.rest

import org.specs._

import com.sun.jersey.api.client._

import org.psug.usi.netty._
import scala.io.{Codec, Source}
import org.psug.usi.service.SimpleRepositoryServices

class WebPageSpec extends SpecificationWithJUnit {

  val listenPort = 12345

  def webResource( path:String ) = new Client().resource("http://localhost:"+listenPort+path)

  val setup = new SpecContext {
    val repositories = new SimpleRepositoryServices
    val webServer : WebServer = new WebServer(listenPort,repositories)

    // start/stop web server on each Specification
    beforeSpec { webServer.start; repositories.start  }
    afterSpec { webServer.stop ; repositories.stop }
    
  }


  def getResponseAndContent( path:String ) = {
    val response = webResource( path ).get(classOf[ClientResponse])
    val content = webResource( path ).get(classOf[String])
    (response, content )
  }


  "web page server" should {
    shareVariables()

    "return a content for a index.html url" in {
      val ( response, content ) = getResponseAndContent( "/web/index.html")
      val expectedContent = Source.fromFile( "./web/index.html" )(Codec.UTF8).mkString

      response.getStatus must be_==( ClientResponse.Status.OK.getStatusCode )
      response.getHeaders.getFirst( "Content-Type" ) must be_==( "text/html; charset=utf-8" )
      content must be_==( expectedContent )
      
    }

    "return a content for a /web/js/psug.js url" in {
      val ( response, content ) = getResponseAndContent( "/web/js/psug.js")
      val expectedContent = Source.fromFile( "./web/js/psug.js" )(Codec.UTF8).mkString

      response.getStatus must be_==( ClientResponse.Status.OK.getStatusCode )
      response.getHeaders.getFirst( "Content-Type" ) must be_==( "text/html; charset=utf-8" )
      content must be_==( expectedContent )

    }

    "return a contennt a / url" in {
      val ( response, content ) = getResponseAndContent( "/web/")
      val expectedContent = Source.fromFile( "./web/index.html" )(Codec.UTF8).mkString

      response.getStatus must be_==( ClientResponse.Status.OK.getStatusCode )
      response.getHeaders.getFirst( "Content-Type" ) must be_==( "text/html; charset=utf-8" )
      content must be_==( expectedContent )

    }

    "return a contennt a '' url" in {
      val ( response, content ) = getResponseAndContent( "/web")
      val expectedContent = Source.fromFile( "./web/index.html" )(Codec.UTF8).mkString

      response.getStatus must be_==( ClientResponse.Status.OK.getStatusCode )
      response.getHeaders.getFirst( "Content-Type" ) must be_==( "text/html; charset=utf-8" )
      content must be_==( expectedContent )

    }

  }
}