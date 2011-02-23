package org.psug.usi.rest.test

import org.psug.usi.system._
import com.sun.jersey.spi.inject._
import javax.ws.rs.ext._
import javax.ws.rs.core._

object fakeSystemExit extends Exit { 
  override def exit = println("Exiting") 
}

@Provider
class ExitProvider extends SingletonTypeInjectableProvider[Context,Exit](classOf[Exit],fakeSystemExit)

