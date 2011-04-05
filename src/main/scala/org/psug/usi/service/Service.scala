package org.psug.usi.service

import org.psug.usi.akka.Receiver
import akka.util.Logging

/**
 * User: alag
 * Date: 2/26/11
 * Time: 5:24 PM
 */

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
 * check akka.conf for host/port
 */
trait Service extends Receiver with Logging {
  val name:String

  def go = {
    start
    registerAsRemoteActor
  }

  private def registerAsRemoteActor {
    log.info( "Register Remote actor " + name )
    register( name )
  }

  override def stop(){
    unregister()
    super.stop()
  }
}
