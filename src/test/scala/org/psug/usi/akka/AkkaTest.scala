package org.psug.usi.akka

import org.junit.Assert._
import org.junit.Test
import akka.actor.Actor
import akka.actor.Actor._
import akka.routing.{ LoadBalancer, CyclicIterator }
import akka.remoteinterface._

/**
 * User: alag
 * Date: 3/14/11
 * Time: 5:24 PM
 */


class SimpleAkkaActor extends Actor {
  def receive = {
    case "OK" =>
      println( "----------------" + self.sender  + " / " + self.channel )
      self.channel ! "K0"
    case _ =>
  }
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
    remote.register("SimpleAkkaActor", actorOf[SimpleAkkaActor])


    val simpleActor = remote.actorFor("SimpleAkkaActor", "localhost", 2552)


    assertEquals( Some( "K0" ),  (simpleActor !! "OK") )


    remote.shutdown
    remote.removeListener(listener)

  }
}
