package org.psug.usi

import _root_.akka.util.Logging
import org.psug.usi.netty.WebServer
import java.util.Properties
import service.{ClientServices, ServerServices}
import java.net.InetAddress

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

    
    val currentHost = InetAddress.getLocalHost().getHostName()

    if( currentHost == servicesHost ){
      // setup http proxy (for twitter client)
      val proxyHost = properties.getProperty("http.proxy.host")
      if( proxyHost != null ){
        System.setProperty("java.net.useSystemProxies", "true");
        System.setProperty("http.proxyHost", proxyHost )
        val proxyPort = properties.getProperty("http.proxy.port")
        if( proxyPort != null ) System.setProperty("http.proxyPort", proxyPort )
      }
    }
    
    val main = new Main
    Runtime.getRuntime().addShutdownHook(new ShutdownHook(main))

    main.start( currentHost, servicesHost, webPort, servicesPort, webAuthenticationKey )

   
  }


  class ShutdownHook(main:Main)  extends Thread with Logging{
    override def run( ) {
      log.info("Shutting down server")
      main.stop()
    }
  }
}

class Main extends Logging {

  var services:ServerServices = null
  var webServer:WebServer = null

  def start( currentHost:String, servicesHost:String, webPort:Int, servicesPort:Int, webAuthenticationKey:String ) = {
    // Host/port conf is in akka.conf
    services = new ServerServices
    services.launch

    // if we are on local service host do not use remoting
    webServer = if( currentHost == servicesHost ){
      new WebServer( webPort, services, webAuthenticationKey )
    }
    else{
      val remoteService = new ClientServices( servicesHost, servicesPort )
      new WebServer( webPort, remoteService, webAuthenticationKey )
    }

    webServer.start
    log.info("Started PSUG USI2011 Challenge Server on "+currentHost+" web port: " + webPort + " service port:" + servicesPort )


  }

  def stop() = {
    if( webServer != null ) webServer.stop
    if( services != null ) services.shutdown

    log.info("Stopped PSUG USI2011 Challenge" )
  }

}
