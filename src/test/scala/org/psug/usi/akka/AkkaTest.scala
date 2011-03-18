package org.psug.usi.akka

import org.junit.Assert._
import org.junit.Test
import akka.actor.Actor
import akka.actor.Actor._
import akka.remoteinterface._

/**
 * User: alag
 * Date: 3/14/11
 * Time: 5:24 PM
 */





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

    

    val simpleReceiver = new Receiver {
      def receive = {
        case "OK" => reply( "K0" )
        case _ =>
      }
    }

    simpleReceiver.start()
    assertEquals( "K0",  (simpleReceiver !? "OK") )


    simpleReceiver.register( "Simple" )

    val remoteReceiver = new RemoteReceiver("Simple", "localhost", 2552)
    assertEquals( "K0",  remoteReceiver !? "OK" )

    val future = remoteReceiver !! "OK"
    future.awaitBlocking
    assertEquals( "K0", future.result.get )

    
    remote.shutdown
    remote.removeListener(listener)

  }
}
