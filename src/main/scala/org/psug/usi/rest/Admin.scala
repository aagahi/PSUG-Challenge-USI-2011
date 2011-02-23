package org.psug.usi.rest

import org.psug.usi.system._

import javax.ws.rs._
import core._
import net.liftweb.json._
import net.liftweb.json.Serialization.write


@Path("/admin")
class Admin {

  implicit val formats = Serialization.formats(NoTypeHints)

  @GET
  @Path("/_status")
  def status() : String = {
    write("Bye")
  }

  @GET
  @Path("/_exit")
  def exit(@Context exiter : Exit) : String = {
    implicit val formats = Serialization.formats(NoTypeHints)
    write("Bye")
  }
}
