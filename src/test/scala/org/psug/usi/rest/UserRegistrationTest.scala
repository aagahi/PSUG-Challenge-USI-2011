package org.psug.usi.rest

import org.junit.Assert._
import org.junit.Test

import org.hamcrest.CoreMatchers._
import org.junit.matchers.JUnitMatchers._

import net.liftweb.json._
import JsonDSL._
import JsonAST._

import com.sun.jersey.api.client._

class UserRegistrationTest extends RESTTestUtilities {

  @Test
  def succeedsIfUserDoesNotExist() = {
    val userDescription = compact(render(("firstName"	-> "Martin") ~ 
					 ("lastName"	-> "Odersky") ~ 
					 ("mail"	-> "m.odersky@scala-lang.org") ~ 
					 ("password"	-> "0xcafebabe")))
    val response = webResource.path("/api/user/").header("Content-Type","application/json").post(classOf[String], userDescription)
    assertThat(response, is("1"))
    val user = webResource.path("/api/user/1").get(classOf[String])
    assertThat(user,is(userDescription))
  }
  
  @Test
  def doesNotSucceedIfUserWithSameEmailExists() = {
    val userDescription = compact(render(("firstName"	-> "Martin") ~ 
					 ("lastName"	-> "Odersky") ~ 
					 ("mail"	-> "m.odersky@scala-lang.org") ~ 
					 ("password"	-> "0xcafebabe")))
    webResource.path("/api/user/").header("Content-Type","application/json").post(classOf[String], userDescription)
    val otherUser = compact(render(("firstName"	-> "Myriam") ~ 
				   ("lastName"	-> "Odersky") ~ 
				   ("mail"	-> "m.odersky@scala-lang.org") ~ 
				   ("password"	-> "0x12345678")))
    try { 
      val response = webResource.path("/api/user/").header("Content-Type","application/json").post(classOf[String], otherUser)
    } catch { 
      case e : UniformInterfaceException => assertThat(e.getResponse.getStatus,is(400))
    }
  }
  
}
