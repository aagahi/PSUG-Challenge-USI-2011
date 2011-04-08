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


trait ActorWrapper extends Logging {
  val actorRef:ActorRef

  def ! ( msg:Any )( implicit senderActor:Option[ActorRef] =None ) = actorRef.!( msg )( senderActor )

  def !? ( msg:Any )(implicit senderActor: Option[ActorRef] =None) =  (actorRef.!!( msg, 30*1000 )( senderActor )) match {
    case Some(x) => x
    case None => throw new RuntimeException("Sending message to actor '%s' timeout. Message: '%s'".format(actorRef.id, msg))
  }
  
  def !! [T]( msg:Any )(implicit senderActor: Option[ActorRef] =None):Future[T] = actorRef.!!!( msg, 20000 )( senderActor )

  def callback( msg:Any )( f: PartialFunction[Any,Unit] )(implicit senderActor: Option[ActorRef] =None){
    val future:Future[Any] = actorRef.!!!( msg )( senderActor )
    future.onComplete( {
      future:Future[Any] =>
        future.result match {
          case Some( v ) => f( v )
          case None => log.error("Unexpected empty futur for callback message: "+ msg )
        }
    } )
  }


  def sender = actorRef.channel
  def start() = actorRef.start
  def reply( msg:Any ) = sender ! msg 
  def stop() = actorRef.stop()
}

class ReceiverAkkaActor( reciever:Receiver ) extends Actor {
  def receive = reciever.receive
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




