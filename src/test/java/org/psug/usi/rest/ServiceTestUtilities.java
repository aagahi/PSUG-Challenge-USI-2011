package org.psug.usi.rest;

import static org.junit.Assert.assertThat;

import org.hamcrest.Matcher;
import org.junit.Before;

import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.test.framework.AppDescriptor;
import com.sun.jersey.test.framework.JerseyTest;
import com.sun.jersey.test.framework.WebAppDescriptor;
import com.sun.jersey.test.framework.spi.container.inmemory.InMemoryTestContainerFactory;

/**
 * 
 * @author abailly@oqube.com
 * @version $Id$
 */
public class ServiceTestUtilities extends JerseyTest {

	protected WebResource webResource;

	public ServiceTestUtilities() throws Exception {
		super(new InMemoryTestContainerFactory());
	}

	@Override
	protected AppDescriptor configure() {
		return new WebAppDescriptor.Builder(restletsPath()).contextPath("")
				.build();
	}

	protected String restletsPath() {
		return "org.psug.usi.rest";
	}

	protected <T> Class<T> as(Class<T> klass) {
		return klass;
	}

	@Before
	public void getResource() throws Exception {
		this.webResource = resource();
	}

	protected class QueryParam {

		String k;
		String v;

		QueryParam(String k, String v) {
			this.k = k;
			this.v = v;
		}

	}

	protected QueryParam withParam(String k, String v) {
		return new QueryParam(k, v);
	}

	protected <T> void assertResponseTo(String url, Class<T> klass,
			Matcher<T> matchExpected) {
		T response = webResource.path(url).get(klass);
		assertThat(response, matchExpected);
	}

	protected <T> void assertResponseTo(String url, Class<T> klass,
			QueryParam q, Matcher<T> matchExpected) {
		T response = webResource.path(url).queryParam(q.k, q.v).get(klass);
		assertThat(response, matchExpected);
	}

}
