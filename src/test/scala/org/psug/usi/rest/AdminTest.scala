package org.psug.usi.rest

import org.junit.Assert._
import org.junit.Test

import org.hamcrest.CoreMatchers._
class AdminTest extends RESTTestUtilities {

  @Test
  def respondsToExitRequest() = {
    val response = webResource.path("/admin/_exit").get(classOf[String])
    assertThat(response, is("\"Bye\""))
  }
}
