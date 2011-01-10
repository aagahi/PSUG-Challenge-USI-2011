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
abstract class ServiceTestUtilities extends JerseyTest(new InMemoryTestContainerFactory()) {

  var webResource: WebResource = _

  override def configure() : AppDescriptor = {
    new WebAppDescriptor.Builder(restletsPath()).contextPath("")
    .build()
  }

  def restletsPath() : String = "org.psug.usi.rest"
  
  @Before
  def getResource() : Unit = {
    webResource = resource()
  }

  def assertResponseTo[T](url : String , klass: Class[T], matchExpected: Matcher[T]) : Unit = {
    val response = webResource.path(url).get(klass)
    assertThat(response, matchExpected)
  }

  def assertResponseTo[T](url : String , klass: Class[T], matchExpected: Matcher[T], param : (String, String)) : Unit = {
    val response = webResource.path(url).queryParam(param._1, param._2).get(klass)
    assertThat(response, matchExpected)
  }

}
