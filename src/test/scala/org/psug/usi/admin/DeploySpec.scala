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
import actors.remote._
import org.psug.usi.store.DataPulled

class DeploySpec extends SpecificationWithJUnit {

  implicit val formats = Serialization.formats(NoTypeHints)

  val listenPort = 34567

  def webResource( path:String ) = new Client().resource("http://localhost:"+listenPort+path)

  val context = new SpecContext {
    val main = new Main
    after(main.stop)
  }

  "main launcher" should {

    "start as web server on given port given arguments 'web'" in {
      context.main.start("Web","" + listenPort)
      val result = webResource("/admin/status").header("Content-Type","application/json").get(classOf[String])
      val status = read[Status](result)
      status.nodeType must be_==("Web")
      status.port must be_==(listenPort)
    }

/*
    "start as service on given port with arguments 'service'"  in {
       context.main.start("Service","" + listenPort)
       val node = Node("localhost",listenPort)
       val actor = RemoteActor.select( node , 'UserRepositoryService)
       val response  = actor !? PullDataByEmail("")
       response.asInstanceOf[DataPulled[Option[User]]] must be_==(DataPulled(None))
    }
    */
  }
}

