package org.psug.usi.domain

/**
 * User: alag
 * Date: 2/17/11
 * Time: 12:19 AM
 */

import org.specs._
import net.liftweb.json._
import net.liftweb.json.Serialization.read
import org.psug.usi.netty.Status

import org.psug.usi.Main
import actors.Actor._
import actors.remote._
import RemoteActor._
import org.psug.usi.service.ServiceStatus
import org.psug.usi.store.{PullData, DataPulled}
import com.sun.jersey.api.client.Client

class DeploySpec extends SpecificationWithJUnit {

  implicit val formats = Serialization.formats(NoTypeHints)

  val webPort: Int = 34567
  val servicesPort: Int = 55554

  def webResource(path: String) = new Client().resource("http://localhost:" + webPort + path)

  def anActorExpects(value : Int) = actor {
        alive(servicesPort)
        register('UserRepositoryService,self)

        receive {
          case PullData( value ) => reply(DataPulled(None))
          case _                 => fail("expected datapull for user Id " + value)
        }

        exit
      }

  val context = new SpecContext {
    val main = new Main
    after(main.stop)
  }

  "main launcher" should {

    "start as web server on given port given arguments 'web'" in {
      context.main.start("Web", "" + webPort, "" + servicesPort)
      val result = webResource("/admin/status").header("Content-Type", "application/json").get(classOf[String])
      val status = read[Status](result)
      status.nodeType must be_==("Web")
      status.port must be_==(webPort)
      // following code commented out because it does not seem to be possible to
      // unbind an actor with some symbol...
/*
      anActorExpects(12)
      val response = webResource("/api/user/" + 12).header("Content-Type", "application/json").get(classOf[ClientResponse])
      response.getStatus must be_==(ClientResponse.Status.BAD_REQUEST.getStatusCode)
*/
    }

    "start as service on given port with arguments 'service'" in {
      context.main.start("Service","" + servicesPort)
      val node = Node("localhost", servicesPort)
      val actor = RemoteActor.select(node, 'UserRepositoryService)
      val status : Some[Status] = (actor !? (2000,ServiceStatus)).asInstanceOf[Some[Status]]
      status.get.nodeType must be_==("Service")
      status.get.port must be_==(servicesPort)
    }

  }
}

