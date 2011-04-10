package org.psug.usi.utils

import org.psug.usi.akka.Receiver
import akka.dispatch.{Future, Dispatchers}

/**
 * User: alag
 * Date: 4/7/11
 * Time: 5:10 PM
 */
object AsyncExecutor{
  def apply() = new AsyncExecutor()
}

class AsyncExecutor extends Receiver {

  actorRef.dispatcher = Dispatchers.newExecutorBasedEventDrivenDispatcher("HttpQuery")
    .setCorePoolSize(256)
    .setMaxPoolSize(256)
    .build
  start()

  // return a futur of execution block
  def execute[T]( f: =>T ):Future[T] = this !! { ()=> f }


  def receive = {
    case f:( ()=>Any ) => reply( f() )
  }
}