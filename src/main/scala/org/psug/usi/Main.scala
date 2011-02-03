package org.psug.usi

import com.sun.jersey.spi.container.servlet.ServletContainer

import org.mortbay.jetty.Server
import org.mortbay.jetty.servlet.Context
import org.mortbay.jetty.servlet.ServletHolder

/**
 * 
 * @author abailly@oqube.com
 * @version $Rev$
 */
object Main {

  def main(args : Array[String]) = {
    val holder = new ServletHolder(classOf[ServletContainer])
    holder.setInitParameter("com.sun.jersey.config.property.resourceConfigClass", "com.sun.jersey.api.core.PackagesResourceConfig")
    holder.setInitParameter("com.sun.jersey.config.property.packages", "org.psug.usi.rest")

    val server = new Server(8082)
    val context = new Context(server, "/", Context.SESSIONS)
    context.addServlet(holder, "/*")
    server.start()
    
    println( "Starting PSUG USI2011 Challenge..." )
    server.join()
  }

}

