package org.psug.usi.netty

import org.psug.usi.users.User
import org.jboss.netty.util.CharsetUtil
import net.liftweb.json.{Serialization, NoTypeHints}
import org.jboss.netty.channel._
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.handler.codec.http._
import net.liftweb.json.Serialization.{read, write}

/**
 * User: alag
 * Date: 2/22/11
 * Time: 1:07 AM
 */


@ChannelHandler.Sharable
class HttpRequestHandler extends SimpleChannelUpstreamHandler {
  implicit val formats = Serialization.formats(NoTypeHints)

  override def messageReceived( ctx:ChannelHandlerContext , event:MessageEvent ) {
    val channel = ctx.getChannel

    val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)
    response.setHeader(HttpHeaders.Names.CONTENT_TYPE, "text/plain; charset=UTF-8")

    event.getMessage match
    {
      case request:HttpRequest => handleRequest( request, response )
      case _ => println( "Unknown message" )
    }

    event.getChannel.write(response)
    event.getChannel.close

  }

  private def handleRequest( request:HttpRequest, response:HttpResponse ){

    val method = request.getMethod
    val queryStringDecoder = new QueryStringDecoder( request.getUri() )
    val content = request.getContent().toString(CharsetUtil.UTF_8)

    ( method, queryStringDecoder.getPath.split('/').tail ) match {

      case ( HttpMethod.GET, Array("api","user",userId) )  =>
        val str = write(User(0,"Martin", "Odersky","m.odersky@scala-lang.org","0xcafebabe"))
        response.setContent(ChannelBuffers.copiedBuffer( str, CharsetUtil.UTF_8))

        
      case ( HttpMethod.POST, Array("api","user") ) =>
        val user = read[User](content)
        if(user.firstName == "Myriam")
          response.setStatus(HttpResponseStatus.BAD_REQUEST)
        else
          response.setContent(ChannelBuffers.copiedBuffer( "1", CharsetUtil.UTF_8))


      case _ =>
        response.setStatus(HttpResponseStatus.NOT_FOUND)

    }



  }



  override def exceptionCaught( ctx:ChannelHandlerContext, e:ExceptionEvent ){
      e.getChannel.close();
  }
}
