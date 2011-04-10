package org.psug.usi.akka

import org.junit.Assert._
import org.junit.Test
import akka.actor.Actor._
import akka.remoteinterface._
import akka.actor.{Channel, Actor}
import akka.dispatch.Dispatchers
import akka.util.Logging
import java.util.concurrent.atomic.AtomicInteger

/**
 * User: alag
 * Date: 3/14/11
 * Time: 5:24 PM
 */



class SimpleReceiverServer extends Receiver {
  start()
  register("SimpleReceiverServer")

  def receive = {
    case Query("OK") => reply( Response("K0") )
    case _ =>
  }
}


case class Query(msg:String)
case class Response(msg:String)

class SimpleReceiverClient extends Receiver {

   actorRef.dispatcher = Dispatchers.newExecutorBasedEventDrivenDispatcher("SimpleReceiverClient")
    .setCorePoolSize(256)
    .setMaxPoolSize(256)
    .build
  start()
  val remoteReceiver = new RemoteReceiver("SimpleReceiverServer", "localhost", 2552 )

  var target:Channel[Any] = null
  def receive = {
    case Query( x ) =>
      target = sender
      Thread.sleep( 500 )
      remoteReceiver ! Query( x )
    case Response( x ) =>
      target ! Response( x )
    case _ =>
  }
}


class LightEventReceiverClient extends Receiver {

  start()
  val remoteReceiver = new RemoteReceiver("SimpleReceiverServer", "localhost", 2552 )

  var target:Channel[Any] = null
  def receive = {
    case Query( x ) =>
      target = sender
      remoteReceiver ! Query( x )
    case Response( x ) =>
      target ! Response( x )
    case _ =>
  }
}


class AkkaTest extends Logging {

  @Test
  def respondsToExitRequest() = {
    val listener = actorOf(new Actor {
      def receive = {
        case RemoteServerStarted(server)                           => println( ">>>> Started: " + server )
        case RemoteServerShutdown(server)                          => println( ">>>> Shutdown: " + server )
        case RemoteServerError(cause, server)                      => println( ">>>> Error: " + server  + " cause: " + cause )
        case RemoteServerClientConnected(server, clientAddress)    => println( ">>>> Connected: " + server + " client " + clientAddress )
        case RemoteServerClientDisconnected(server, clientAddress) => println( ">>>> Disconnected: " + server + " client " + clientAddress )
        case RemoteServerClientClosed(server, clientAddress)       => println( ">>>> Closed: " + server + " client " + clientAddress )
        case RemoteServerWriteFailed(request, cause, server, clientAddress) => println( ">>>> Writefail: " + server +" cause: " + cause  + " client " + clientAddress )
      }
    }).start

    remote.addListener( listener )
    remote.start()

    val simpleServer = new SimpleReceiverServer
    
    val simpleReceivers = (1 to 256) map{ i=> new SimpleReceiverClient }
    val t0 = System.currentTimeMillis
    simpleReceivers.map( _ !! Query("OK") ).foreach{
      future =>
        assertEquals( Some( Response( "K0" ) ), future.awaitBlocking.result )
    }
    val t1 = System.currentTimeMillis

    assert( (t1-t0) < 5000 )

    val t2 = System.currentTimeMillis
    val count = new AtomicInteger(0)
    val numFutures = 10000
    (0 until numFutures).map( i=> new LightEventReceiverClient )
                .map( _.callback( Query("OK") ){
                  case Response( "K0" ) => count.incrementAndGet; Unit
                })
    val t3 = System.currentTimeMillis

    while( count.get < numFutures) Thread.sleep(10)
    log.info( "Future timing " + (t3-t2)+ " / " + count.get )
    assert( count.get == numFutures  )

    remote.shutdown
    remote.removeListener(listener)

  }
}
