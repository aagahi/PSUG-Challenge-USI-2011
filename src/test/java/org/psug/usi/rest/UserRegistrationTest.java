package org.psug.usi.rest;

import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.Before;
import org.junit.After;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.matchers.JUnitMatchers.*;


/**
 * 
 * @author abailly@oqube.com
 * @version $Id$
 */
public class UserRegistrationTest extends ServiceTestUtilities {

  public UserRegistrationTest() throws Exception {}
 
  @Test
  public void succeedsIfUserDoesNotExist() throws Exception {
    String userDescription = "{\"firstname\": \"Martin\", \"lastname\": \"Odersky\", \"mail\": \"m.odersky@scala-lang.org\", \"password\": \"0xcafebabe\"}";
    String response = webResource.path("/api/user").post(String.class, userDescription);
    assertThat(response, is(""));
  }
 
}
