package org.psug.usi

import _root_.akka.util.Logging
import org.psug.usi.netty.WebServer
import service.{RepositoryServices, RemoteServices, SimpleRepositoryServices}

/**
 * 
 * @author abailly@oqube.com
 * @version $Rev$
 */

/**
 * Aslo check resource/akka.conf for akka server configuration
 * We can start manual the akka remote actor server using
 * remote.start("localhost", 2552)
 * see http://doc.akka.io/remote-actors-scala
 */

object Main {

  val WEB_AUTHICATION_KEY = "dummy"
  val DEFAULT_PORT = "8082"
  val DEFAULT_SERVICE_PORT = "2552"

  def main(args : Array[String]) = {
    val port = if(args.length > 0) args(0) else DEFAULT_PORT
    val serviceport = if(args.length > 1) args(1) else DEFAULT_SERVICE_PORT

    val main = new Main
    main.start( "Web", port, serviceport )

    while(System.in.read() == -1) wait(500)
    main.stop
  }

}

class Main extends Logging {

  trait Agent  {
    val name  : String
    val port  : Int
    def start : Unit
    def stop  : Unit
  }

  var agent : Agent = null
  var services:RepositoryServices = null

  def start(args : String*) = {
    val webport : Int = args(1).toInt
    val servicesPort = args(2).toInt
    args(0) match {
      case "Web" =>
        services = new SimpleRepositoryServices(servicesPort)
        services.start
        agent = new WebServer(webport,services) with Agent {
          val name =  "Web"
          val port = webport
        }
      case "Service" =>
        agent = new SimpleRepositoryServices(servicesPort) with Agent {
          val name = "Services"
          val port = webport
        }
    }

    agent.start

    log.info("Started PSUG USI2011 Challenge " + agent.name  +" agent  at 0.0.0.0:" + agent.port)
  }

  def stop() = {
    agent.stop
    if( services != null )  services.stop
    log.info("Stopped PSUG USI2011 Challenge " + agent.name + " at 0.0.0.0:" + agent.port)
  }

}
