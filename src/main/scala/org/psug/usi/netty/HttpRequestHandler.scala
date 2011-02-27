package org.psug.usi.netty

import org.psug.usi.domain._
import org.jboss.netty.util.CharsetUtil
import net.liftweb.json.{Serialization, NoTypeHints}
import org.jboss.netty.channel._
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.handler.codec.http._
import net.liftweb.json.Serialization.{read, write}
import actors.Actor
import org.psug.usi.service.UserRepositoryService


/**
 * User: alag
 * Date: 2/22/11
 * Time: 1:07 AM
 */

class RequestActor extends Actor{
  import org.psug.usi.domain.UserRepository._

  implicit val formats = Serialization.formats(NoTypeHints)

  var channel:Channel = null

  start
  def act {
    loop {
      react {
        case event:MessageEvent =>
          channel = event.getChannel
          event.getMessage match
          {
            case request:HttpRequest => handleRequest( request )
            case _ => println( "Unknown message" )
          }

        case DataStored( Right( data ) ) =>  sendResponse( Some( data ), HttpResponseStatus.OK )
        case DataStored( Left( message ) ) => sendResponse( None, HttpResponseStatus.BAD_REQUEST )

        case DataPulled( Some( data ) ) =>  sendResponse( Some( data ), HttpResponseStatus.OK )
        case DataPulled( None ) => sendResponse( None, HttpResponseStatus.BAD_REQUEST )

      }
    }
  }



  private def handleRequest( request:HttpRequest ){

    val method = request.getMethod
    val queryStringDecoder = new QueryStringDecoder( request.getUri() )
    val content = request.getContent().toString(CharsetUtil.UTF_8)

    ( method, queryStringDecoder.getPath.split('/').tail ) match {

      case ( HttpMethod.GET, Array("api","user",userId) )  =>
        UserRepositoryService.remoteRef ! PullData( userId.toInt )

      case ( HttpMethod.POST, Array("api","user") ) =>
        val user = read[User](content)
        UserRepositoryService.remoteRef ! StoreData(user)

    }
  }


  private def sendResponse( value:Option[AnyRef], status:HttpResponseStatus ){
    val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, status )

    value.foreach{
      data =>
      val str = write( data )
      response.setHeader(HttpHeaders.Names.CONTENT_TYPE, "application/json; charset=UTF-8")
      response.setContent(ChannelBuffers.copiedBuffer( str, CharsetUtil.UTF_8))
    }

    val future = channel.write(response)
    future.addListener(ChannelFutureListener.CLOSE)
  }
}

@ChannelHandler.Sharable
class HttpRequestHandler extends SimpleChannelUpstreamHandler {

  override def messageReceived( ctx:ChannelHandlerContext , e:MessageEvent ) {
    new RequestActor() ! e
  }




  override def exceptionCaught( ctx:ChannelHandlerContext, e:ExceptionEvent ){
      e.getChannel.close
  }
}
