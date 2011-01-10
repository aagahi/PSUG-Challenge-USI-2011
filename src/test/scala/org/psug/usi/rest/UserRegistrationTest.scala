package org.psug.usi.rest

import org.junit.Assert._
import org.junit.Test

import org.hamcrest.CoreMatchers._
import org.junit.matchers.JUnitMatchers._

import net.liftweb.json._
import JsonDSL._
import JsonAST._

class UserRegistrationTest extends RESTTestUtilities {

  @Test
  def succeedsIfUserDoesNotExist() = {
    val userDescription = compact(render(("firstname"	-> "Martin") ~ 
					 ("lastname"	-> "Odersky") ~ 
					 ("mail"	-> "m.odersky@scala-lang.org") ~ 
					 ("password"	-> "0xcafebabe")))
    val response = webResource.path("/api/user/").header("Content-Type","application/json").post(classOf[String], userDescription)
    assertThat(response, is("1"))
  }
  
}
