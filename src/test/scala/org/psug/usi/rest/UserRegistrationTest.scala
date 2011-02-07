package org.psug.usi.rest

import org.psug.usi.users._

import org.junit.Assert._
import org.junit.Test

import org.hamcrest.CoreMatchers._
import org.junit.matchers.JUnitMatchers._

import net.liftweb.json._
import JsonDSL._
import JsonAST._

import com.sun.jersey.api.client._

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

class UserRegistrationTest extends RESTTestUtilities {
  implicit val formats = Serialization.formats(NoTypeHints)

  val martinOdersky = User(0,"Martin", "Odersky","m.odersky@scala-lang.org","0xcafebabe")
  val myriamOdersky = User(0,"Myriam", "Odersky","m.odersky@scala-lang.org","0xbabecafe")

  @Test
  def succeedsIfUserDoesNotExist() = {
    val response = webResource.path("/api/user/").header("Content-Type","application/json").post(classOf[String], write(martinOdersky))
    assertThat(response, is("1"))
    val user = webResource.path("/api/user/1").get(classOf[String])
    assertThat(read[User](user),is(martinOdersky))
  }
  
  @Test
  def doesNotSucceedIfUserWithSameEmailExists() = {
    webResource.path("/api/user/").header("Content-Type","application/json").post(classOf[String], write(martinOdersky))
    try { 
      val response = webResource.path("/api/user/").header("Content-Type","application/json").post(classOf[String], write(myriamOdersky))
    } catch { 
      case e : UniformInterfaceException => assertThat(e.getResponse.getStatus,is(400))
    }
  }
  
}
