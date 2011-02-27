package org.psug.usi.service

import actors.remote.{RemoteActor, Node}
import actors.Actor

/**
 * User: alag
 * Date: 2/26/11
 * Time: 5:24 PM
 */

abstract class RemoteService extends Actor {
  val port = 55555
  val host = "localhost"
  val symbol = Symbol(getClass.getName)


  start
  registerAsRemoteActor

  def registerAsRemoteActor {
    println( "Register Remote actor " + symbol + " host: " + host  + " port: " + port )
    RemoteActor.alive( port )
    RemoteActor.register( symbol, this )
  }
  
  lazy val remoteRef = RemoteActor.select( Node( host, port ), symbol )



}