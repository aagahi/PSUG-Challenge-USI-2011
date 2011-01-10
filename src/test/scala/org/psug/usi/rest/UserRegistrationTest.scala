package org.psug.usi.rest

import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.junit.After

import org.hamcrest.CoreMatchers._
import org.junit.matchers.JUnitMatchers._

class UserRegistrationTest extends RESTTestUtilities {

  @Test
  def succeedsIfUserDoesNotExist() = {
    def userDescription = "{\"firstname\": \"Martin\", \"lastname\": \"Odersky\", \"mail\": \"m.odersky@scala-lang.org\", \"password\": \"0xcafebabe\"}"
    def response = webResource.path("/api/user/").header("Content-Type","application/json").post(classOf[String], userDescription)
    assertThat(response, is("1"))
  }
 
}
