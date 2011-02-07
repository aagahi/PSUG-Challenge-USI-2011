package org.psug.usi.rest

import org.junit.Assert._
import org.junit.Test

import org.hamcrest.CoreMatchers._
import org.junit.matchers.JUnitMatchers._

import net.liftweb.json._
import JsonDSL._
import JsonAST._

class AdminTest extends RESTTestUtilities {

  @Test
  def respondsToExitRequest() = {
    val response = webResource.path("/admin/_exit").get(classOf[String])
    assertThat(response, is("\"Bye\""))
  }
}
