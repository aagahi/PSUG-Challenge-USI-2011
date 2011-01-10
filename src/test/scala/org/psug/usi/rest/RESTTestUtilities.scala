package org.psug.usi.rest;

import org.junit.Assert.assertThat

import org.hamcrest.Matcher
import org.junit.Before

import com.sun.jersey.api.client.WebResource
import com.sun.jersey.test.framework.AppDescriptor
import com.sun.jersey.test.framework.JerseyTest
import com.sun.jersey.test.framework.WebAppDescriptor
import com.sun.jersey.test.framework.spi.container.inmemory.InMemoryTestContainerFactory

/**
 * 
 * @author abailly@oqube.com
 * @version $Id$
 */
abstract class RESTTestUtilities extends JerseyTest(new InMemoryTestContainerFactory()) {

  var webResource : WebResource = _

  @Before 
  def defineResource = {
    webResource = resource()
  }

  override def configure() : AppDescriptor = {
    new WebAppDescriptor.Builder(restletsPath()).contextPath("")
    .build()
  }

  def restletsPath() : String = "org.psug.usi.rest"
  
}
