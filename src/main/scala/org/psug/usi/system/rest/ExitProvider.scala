package org.psug.usi.system.rest

import org.psug.usi.system._
import com.sun.jersey.spi.inject._
import javax.ws.rs.ext._
import javax.ws.rs.core._
import java.lang.reflect.Type

@Provider
class ExitProvider extends SingletonTypeInjectableProvider[Context,Exit](classOf[Exit],systemExit)

