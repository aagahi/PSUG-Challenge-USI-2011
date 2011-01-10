package org.psug.usi.rest

import javax.ws.rs._

@Path("/api/user")
class UserApi {

  @POST
  @Consumes(Array("application/json"))
  @Produces(Array("text/plain"))
  def register(userDescription: String) = {
    ""
  }

}
