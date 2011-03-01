package org.psug.usi

import com.sun.jersey.spi.container.servlet.ServletContainer

import org.psug.usi.netty.WebServer

/**
 * 
 * @author abailly@oqube.com
 * @version $Rev$
 */
object Main {

  val DEFAULT_PORT = "8082"

  def main(args : Array[String]) = {
    val port = if(args.length > 0) args(0) else DEFAULT_PORT

    val server = new WebServer(Integer.parseInt(port))
    
    println("Started PSUG USI2011 Challenge server at 0.0.0.0:" + port)
  }

}

