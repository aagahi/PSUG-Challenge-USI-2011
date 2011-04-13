package org.psug.usi.akka

/**
 * User: alag
 * Date: 3/18/11
 * Time: 10:15 AM
 */

import scala.PartialFunction
import akka.actor.{ActorRef, Actor}
import akka.actor.Actor._
import akka.dispatch.Future
import akka.util.Logging

case class CallbackQuery( msg:AnyRef, target:ActorRef, f: PartialFunction[Any,Unit] )

class Callback extends Actor{

    def receive = {
      case CallbackQuery( msg, target, f ) =>
        val future:Future[AnyRef] = target.!!!( msg, 20000 )( None )
        future.onComplete( {
          future:Future[AnyRef] =>
          future.result match {
            case Some( v ) => f( v )
            case None => log.error("Unexpected empty futur for callback message: "+ msg )
          }
        } )

    }
  }

trait ActorWrapper extends Logging {
  val actorRef:ActorRef

  def ! ( msg:Any )( implicit senderActor:Option[ActorRef] =None ) = actorRef.!( msg )( senderActor )

  def !? ( msg:Any )(implicit senderActor: Option[ActorRef] =None) =  (actorRef.!!( msg, 30*1000 )( senderActor )) match {
    case Some(x) => x
    case None => throw new RuntimeException("Sending message to actor '%s' timeout. Message: '%s'".format(actorRef.id, msg))
  }
  
  def !! [T]( msg:Any )(implicit senderActor: Option[ActorRef] =None):Future[T] = actorRef.!!!( msg, 20000 )( senderActor )



  lazy val callbacker = actorOf[Callback]
  def callback( msg:AnyRef )( f: PartialFunction[Any,Unit] ){
    if( callbacker.isUnstarted ) callbacker.start()
    callbacker ! CallbackQuery( msg, actorRef, f )
  }

  
  def sender = actorRef.channel
  def start() = actorRef.start
  def reply( msg:Any ) = sender ! msg 
  def stop() = {
    actorRef.stop()
    if( callbacker.isRunning ) callbacker.stop()
  }
}

class ReceiverAkkaActor( reciever:Receiver ) extends Actor {
  def receive:PartialFunction[Any,Unit] = {
    case x:Any =>
      try {
        reciever.receive( x )
      }
      catch {
        case e:Throwable => log.error( e, e.getMessage )
      }
  }
}


trait Receiver extends ActorWrapper {
  override val actorRef:ActorRef = actorOf( new ReceiverAkkaActor( this ) )
  implicit val implicitActorRef = Some( actorRef )

  def receive:PartialFunction[Any,Unit]
  // check resource/akka.conf for server configuration
  def register( id:String ){ remote.register( id, actorRef ) }
  def unregister(){ remote.unregister( actorRef ) }

}


class RemoteReceiver( id:String, host:String, port:Int) extends ActorWrapper {
  val actorRef:ActorRef = remote.actorFor( id, host, port )
}




