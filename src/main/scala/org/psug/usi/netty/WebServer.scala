package org.psug.usi.netty

import org.jboss.netty.bootstrap.ServerBootstrap
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory

import java.net.InetSocketAddress
import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.http.{HttpResponseEncoder, HttpRequestDecoder}
import java.util.concurrent.Executors


/**
 * User: alag
 * Date: 2/22/11
 * Time: 12:46 AM
 */

object WebServer{
  lazy val defaultWebServer = new WebServer

  val ioBufferSize = 1024*8
  val keepAlive = true
  val backLog = 5000
  val tcpNoDelay = true
}


class WebServer( val listenPort:Int = 18080 )  {
  import  WebServer._
  val bootstrap = new ServerBootstrap(
                                    new NioServerSocketChannelFactory(
                                    Executors.newCachedThreadPool(),
                                    Executors.newCachedThreadPool()))


  bootstrap.setOption("child.tcpNoDelay", tcpNoDelay );
  bootstrap.setOption("child.receiveBufferSize", ioBufferSize )
  bootstrap.setOption("child.sendBufferSize", ioBufferSize )
  bootstrap.setOption("child.keepAlive", keepAlive )
  bootstrap.setOption( "backlog", backLog )


  bootstrap.setPipelineFactory( new HttpServerPipelineFactory() )
  bootstrap.bind( new InetSocketAddress( "0.0.0.0", listenPort ) )

}



class HttpServerPipelineFactory extends ChannelPipelineFactory {

    def getPipeline() = {
        val pipeline = Channels.pipeline()
        pipeline.addLast("decoder", new HttpRequestDecoder());
        pipeline.addLast("encoder", new HttpResponseEncoder());
        pipeline.addLast("handler", new HttpRequestHandler())
        pipeline
      }
}

