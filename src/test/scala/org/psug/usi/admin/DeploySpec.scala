package org.psug.usi.domain

/**
 * User: alag
 * Date: 2/17/11
 * Time: 12:19 AM
 */

import org.specs._
import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import com.sun.jersey.api.client.Client

import org.psug.usi.system._

import org.psug.usi.Main

class DeploySpec extends SpecificationWithJUnit {

  implicit val formats = Serialization.formats(NoTypeHints)

  val listenPort = 34567

  def webResource( path:String ) = new Client().resource("http://localhost:"+listenPort+path)

  "main launcher" should {

    "start as web server on given port given arguments 'web'" in {
      val main = new Main
      main.start("web","" + listenPort)
      val result = webResource("/admin/status").header("Content-Type","application/json").get(classOf[String])
      println(result)
      val status = read[Status](result)
      status.nodeType must be_==("Web")
      status.port must be_==(listenPort)
      main.stop
    }
  }
}

