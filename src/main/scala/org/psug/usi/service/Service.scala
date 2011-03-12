package org.psug.usi.service

import actors.remote.{RemoteActor, Node}
import actors.{DaemonActor, AbstractActor}

/**
 * User: alag
 * Date: 2/26/11
 * Time: 5:24 PM
 */

trait ServiceConfiguration {
  val port :  Int
  val host  : String
  val symbol : Symbol
}

trait DefaultServiceConfiguration extends ServiceConfiguration {
  override lazy val port = 55555
  override lazy val host = "localhost"
}

/**
 * Request for status of the service.
 */
case object ServiceStatus

/**
 * Unconditional stop message.
 * All services should stop() when receiving this message.
 */
case object Exit


/**
 * A service is an actor that is registered for remote access.
 */
trait Service extends DaemonActor {
  config : ServiceConfiguration =>

  def go = {
    start
    registerAsRemoteActor
  }

  def registerAsRemoteActor {
    println( "Register Remote actor " + symbol + " host: " + host  + " port: " + port )
    RemoteActor.alive( port )
    RemoteActor.register( symbol, this )
  }
}

/**
 * A remote service exposes a remote actor.
 */
trait RemoteService {
  self : ServiceConfiguration =>

  lazy val remote: AbstractActor = RemoteActor.select( Node( host, port ), symbol )

}