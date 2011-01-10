package org.psug.usi.rest

import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.junit.After

import org.hamcrest.CoreMatchers._
import org.junit.matchers.JUnitMatchers._


/**
 * 
 * @author abailly@oqube.com
 * @version $Id$
 */
class UserRegistrationTest extends ServiceTestUtilities {

  @Test
  def succeedsIfUserDoesNotExist() = {
    def userDescription = "{\"firstname\": \"Martin\", \"lastname\": \"Odersky\", \"mail\": \"m.odersky@scala-lang.org\", \"password\": \"0xcafebabe\"}"
    def response = webResource.path("/api/user/").post(classOf[String], userDescription)
    assertThat(response, is(""))
  }
 
}
