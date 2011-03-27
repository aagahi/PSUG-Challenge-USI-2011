package org.psug.usi.service

import org.psug.usi.akka.{RemoteReceiver, Receiver}
import akka.util.Logging

/**
 * User: alag
 * Date: 2/26/11
 * Time: 5:24 PM
 */

trait ServiceConfiguration {
  val port :  Int
  val host  : String
  val name : String
}


// Aslo check resource/akka.conf for akka server configuration
trait DefaultServiceConfiguration extends ServiceConfiguration {
  override lazy val port = 2552
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
case object StopReceiver


/**
 * A service is an actor that is registered for remote access.
 */
trait Service extends Receiver with Logging {
  config : ServiceConfiguration =>

  def go = {
    start
    registerAsRemoteActor
  }

  def registerAsRemoteActor {
    log.info( "Register Remote actor " + name )
    register( name )
  }

  override def stop(){
    unregister()
    super.stop()
  }
}

/**
 * A remote service exposes a remote actor.
 */
trait RemoteService {
  self : ServiceConfiguration =>

  lazy val remote = new RemoteReceiver( name, host, port );

}