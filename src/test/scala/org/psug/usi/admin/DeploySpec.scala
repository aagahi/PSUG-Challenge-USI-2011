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
import org.psug.usi.service.ServiceStatus
import org.psug.usi.store.{PullData, DataPulled}
import com.sun.jersey.api.client.Client
import org.psug.usi.akka.{Receiver, RemoteReceiver}
class DeploySpec extends SpecificationWithJUnit {

  implicit val formats = Serialization.formats(NoTypeHints)

  val webPort: Int = 12345
  // Check resources/akka.conf
  val servicesPort: Int = 2552
  val hostname = "localhost"
  val webAuthenticationKey = "dummy"


  def webResource(path: String) = new Client().resource("http://"+hostname+":" + webPort + path )

  def anActorExpects(value : Int) = new Receiver {

        register("UserRepositoryService")

         def receive = {
          case PullData( value ) => reply(DataPulled(None))
          case _                 => fail("expected datapull for user Id " + value)
        }

        //exit
      }.start

  val context = new SpecContext {
    val main = new Main
    after(main.stop)
  }

  "main launcher" should {

    "launch as web server on given port given arguments 'web'" in {
      context.main.start( hostname, webPort, servicesPort, webAuthenticationKey )
      val result = webResource("/admin/status").header("Content-Type", "application/json").get(classOf[String])
      val status = read[Status](result)
      status.nodeType must be_==("Web")
      status.port must be_==(webPort)
    }

    "start as service on given port with arguments 'service'" in {
      context.main.start("Service","" + webPort, "" + servicesPort)
      val remoteReceiver = new RemoteReceiver("UserRepositoryService", "localhost", servicesPort)
      val status = (remoteReceiver !? ServiceStatus).asInstanceOf[Status]
      status.nodeType must be_==("Service")
      status.port must be_==(servicesPort)
    }

  }
}

