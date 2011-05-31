package org.psug.usi.netty

import org.jboss.netty.bootstrap.ServerBootstrap
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory

import java.net.InetSocketAddress
import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.http.{HttpResponseEncoder, HttpRequestDecoder}
import java.util.concurrent.Executors

import org.psug.usi.service._
import akka.util.Logging


/**
 * User: alag
 * Date: 2/22/11
 * Time: 12:46 AM
 */

object WebServer{
  lazy val defaultWebServer = new WebServer

  val ioBufferSize = 1024*8
  val keepAlive = false
  val backLog = 5000
  val tcpNoDelay = false
}


class WebServer( val listenPort:Int = 18080, val services : Services = new ClientServices, webAuthenticationKey:String = "") extends Logging {
  import  WebServer._

  var bootstrap : ServerBootstrap = _
  
  def start : Unit = {
    // IPv4 stack still better than IPv6
    val props = System.getProperties()
    props.setProperty( "java.net.preferIPv4Stack","true" )
    System.setProperties(props)

    bootstrap = new ServerBootstrap(
      new NioServerSocketChannelFactory(
        Executors.newCachedThreadPool(),
        Executors.newCachedThreadPool()))

    bootstrap.setOption("child.tcpNoDelay", tcpNoDelay );
    bootstrap.setOption("child.receiveBufferSize", ioBufferSize )
    bootstrap.setOption("child.sendBufferSize", ioBufferSize )
    bootstrap.setOption("child.keepAlive", keepAlive )
    bootstrap.setOption( "backlog", backLog )

    bootstrap.setPipelineFactory( new HttpServerPipelineFactory(services, webAuthenticationKey) )
    bootstrap.bind( new InetSocketAddress( "0.0.0.0", listenPort ) )

    log.info("Started PSUG USI2011 Challenge web server at 0.0.0.0:" + listenPort)
  }

  def stop = {
    bootstrap.releaseExternalResources
  }
}

class HttpServerPipelineFactory(services : Services, webAuthenticationKey:String ) extends ChannelPipelineFactory {

  def getPipeline() = {
    val pipeline = Channels.pipeline()
    pipeline.addLast("decoder", new HttpRequestDecoder());
    pipeline.addLast("encoder", new HttpResponseEncoder());
    pipeline.addLast("handler", new HttpRequestHandler(services, webAuthenticationKey))
    pipeline
  }
}

