package org.psug.usi.akka

import org.junit.Assert._
import org.junit.Test
import akka.actor.Actor._
import akka.routing.{ LoadBalancer, CyclicIterator }
import akka.remoteinterface._
import scala.PartialFunction
import akka.actor.{ActorRef, Actor}

/**
 * User: alag
 * Date: 3/14/11
 * Time: 5:24 PM
 */

trait AkkaActorWrapper {
  val actorRef:ActorRef
  def !( msg:Any ) = actorRef ! msg
  def !?( msg:Any ) = ( actorRef !! (msg, 10000000) ).get
  def !?( msec: Long, msg: Any ) = actorRef !!( msg, msec )
  def !!( msg:Any ) = ( actorRef !!! msg )
  def sender = actorRef.channel
  def start() = actorRef.start
}

trait Receiver {
  def receive:PartialFunction[Any,Unit]
}

class RemoteReceiver( id:String, host:String, port:Int) extends AkkaActorWrapper {
  override val actorRef:ActorRef = remote.actorFor("Simple", "localhost", 2552 )
}

class SimpleReciever extends Receiver with AkkaActorWrapper {
  override val actorRef:ActorRef = actorOf( new ReceiverAkkaActor( this ) )


  def receive = {
    case "OK" => sender ! "K0"
    case _ =>
  }

}

class ReceiverAkkaActor( reciever:Receiver ) extends Actor {
  def receive = reciever.receive
}







class AkkaTest  {

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

    

    val simpleReceiver = new SimpleReciever
    simpleReceiver.actorRef.start()
    assertEquals( "K0",  (simpleReceiver !? "OK") )


    remote.register( "Simple", simpleReceiver.actorRef )


    val remoteReceiver = new RemoteReceiver("Simple", "localhost", 2552)
    assertEquals( "K0",  (remoteReceiver !? "OK") )


    
    remote.shutdown
    remote.removeListener(listener)

  }
}
