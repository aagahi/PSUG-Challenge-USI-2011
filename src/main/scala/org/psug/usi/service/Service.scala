package org.psug.usi.service

import akka.util.Logging
import org.psug.usi.akka.Receiver

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
 * Unconditional shutdown message.
 * All services should shutdown() when receiving this message.
 */
case object StopReceiver


/**
 * A service is an actor that is registered for remote access.
 * check akka.conf for host/port
 */
trait Service extends Logging {
  self:Receiver =>

  val name:String

  def launch = {
    self.start
    log.info( "Register Remote actor " + name )
    register( name )

  }

  def shutdown(){
    unregister()
    self.stop()
    log.info( "Unregister Remote actor " + name )
    
  }
}
