package org.psug.usi.rest

import javax.ws.rs._
import core._

@Path("/api/user")
class UserApi {

  @POST
  @Consumes(Array("application/json"))
  @Produces(Array("text/plain"))
  def register(userDescription: String) = {
    Response.created(UriBuilder.fromPath("/").segment("1").build()).entity("1").build
  }

}
