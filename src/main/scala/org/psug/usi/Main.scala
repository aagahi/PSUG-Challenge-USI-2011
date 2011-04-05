package org.psug.usi

import _root_.akka.util.Logging
import org.psug.usi.netty.WebServer
import java.util.Properties
import service.{ClientServices, ServerServices, SimpleRepositoryServices}

/**
 * 
 * @author abailly@oqube.com
 * @version $Rev$
 */

/**
 * Aslo check resource/akka.conf for akka server configuration
 * We can launch manual the akka remote actor server using
 * remote.launch("localhost", 2552)
 * see http://doc.akka.io/remote-actors-scala
 */

object Main {

  def main(args : Array[String]) = {

    val properties = new Properties()
    properties.load( getClass.getResourceAsStream( "/configuration.properties" ) )
    val webPort = properties.getProperty("http.port").toInt
    val servicesPort = properties.getProperty("services.port").toInt
    val servicesHost = properties.getProperty("services.host")
    val webAuthenticationKey = properties.getProperty("web.authentication.key")
    val main = new Main
    main.start( servicesHost, webPort, servicesPort, webAuthenticationKey )

    while(System.in.read() == -1) wait(500)
    main.stop
  }

}

class Main extends Logging {

  var services:ServerServices = null
  var webServer:WebServer = null

  def start( servicesHost:String, webPort:Int, servicesPort:Int, webAuthenticationKey:String ) = {
    // Host/port conf is in akka.conf
    services = new SimpleRepositoryServices
    services.launch
    
    val remoteService = new ClientServices( servicesHost, servicesPort )
    webServer = new WebServer( webPort, remoteService, webAuthenticationKey )
    webServer.start
    log.info("Started PSUG USI2011 Challenge Server at 0.0.0.0 web port: " + webPort + " service port:" + servicesPort )
  }

  def stop() = {
    if( webServer != null ) webServer.stop
    if( services != null ) services.shutdown

    log.info("Stopped PSUG USI2011 Challenge at 0.0.0.0" )
  }

}
