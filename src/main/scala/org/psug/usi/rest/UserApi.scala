package org.psug.usi.rest

import javax.ws.rs._
import core._
import Response._

import org.psug.usi.users._

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

/*
 * Spec: Création d'un utilisateur
 *
 *  * URI : .../api/user
 *  * méthode POST
 *  * Paramètre d'entrée 
 *
 * {
 *   "firstname" : "string",
 *   "lastname" : "string",
 *   "email" : "string",
 *   "password" : "string"
 * }
 *
 *  * Codes de retour
 *        o OK : CREATED 201
 *        o Erreur : 400
 *  * Commentaires : si un utilisateur ayant la même adresse email existe déjà, une erreur est retournée.
 *  
 */

@Path("/api/user")
class UserApi {

  implicit val formats = Serialization.formats(NoTypeHints)

  @POST
  @Consumes(Array("application/json"))
  @Produces(Array("text/plain"))
  def register(userDescription: String) = {
    val user = read[User](userDescription)
    if(user.firstName == "Myriam")
      Response.status(Status.BAD_REQUEST).entity("User already exists").build();
    else
      Response.created(UriBuilder.fromPath("/").segment("1").build()).entity("1").build
  }

  @GET
  @Path("{userId}")
  @Produces(Array("application/json"))
  def user(@PathParam("userId") uid : String) : String = {
    write(User(0,"Martin", "Odersky","m.odersky@scala-lang.org","0xcafebabe"))
  }
}
