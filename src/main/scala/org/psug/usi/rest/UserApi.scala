package org.psug.usi.rest

import javax.ws.rs._
import core._

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
 *   "mail" : "string",
 *   "password" : "string"
 * }
 *
 *  * Codes de retour
 *        o OK : CREATED 201
 *        o Erreur : 400
 *  * Commentaires : si un utilisateur ayant la même adresse mail existe déjà, une erreur est retournée.
 *  
 */

case class User(firstname : String, lastname : String, mail : String, password : String)


@Path("/api/user")
class UserApi {

  @POST
  @Consumes(Array("application/json"))
  @Produces(Array("text/plain"))
  def register(userDescription: String) = {
    Response.created(UriBuilder.fromPath("/").segment("1").build()).entity("1").build
  }

  @GET
  @Path("{userId}")
  @Produces(Array("application/json"))
  def user(@PathParam("userId") uid : String) : String = {
    import net.liftweb.json._
    import net.liftweb.json.Serialization.{read, write}
    implicit val formats = Serialization.formats(NoTypeHints)
    write(User("Martin", "Odersky","m.odersky@scala-lang.org","0xcafebabe"))
  }
}
