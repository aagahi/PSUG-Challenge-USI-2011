package org.psug.usi.netty

import org.psug.usi.domain._
import org.jboss.netty.util.CharsetUtil
import net.liftweb.json.{Serialization, NoTypeHints}
import org.jboss.netty.channel._
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.handler.codec.http._
import net.liftweb.json.Serialization.{read, write}
import org.psug.usi.store.{StoreData, PullData, DataPulled, DataStored}
import org.psug.usi.service.{InitGame, Services}
import org.psug.usi.akka.Receiver


/**
 * User: alag
 * Date: 2/22/11
 * Time: 1:07 AM
 */

case class Status( nodeType:String, port:Int )

class RequestActor(services : Services) extends Receiver {

  import services._
  import org.psug.usi.Main._
  implicit val formats = Serialization.formats(NoTypeHints)

  var channel:Channel = null


  def receive = {
    case event:MessageEvent =>
      channel = event.getChannel
      event.getMessage match
      {
        case request:HttpRequest => handleRequest( request )
        case _ => println( "Unknown message" )
      }

    case DataStored( Right( data ) )	=>  sendResponse( Some( data ), HttpResponseStatus.OK )
    case DataStored( Left( message ) )	=> println(message); sendResponse( None, HttpResponseStatus.BAD_REQUEST )

    case DataPulled( Some( data ) )		=>  sendResponse( Some( data ), HttpResponseStatus.OK )
    case DataPulled( None )			=> sendResponse( None, HttpResponseStatus.BAD_REQUEST )

    case UserAuthenticated (Left(user)) =>    sendResponse( None, HttpResponseStatus.CREATED, (HttpHeaders.Names.SET_COOKIE, encodeUserAsCookie(user)))
    case UserAuthenticated (Right(message)) => sendResponse( Some(message), HttpResponseStatus.UNAUTHORIZED)
  }

  private def encodeUserAsCookie(user : User) = {
    val encoder = new CookieEncoder(true)
    encoder.addCookie("session_key", AuthenticationToken.encrypt(AuthenticationToken(user.id,user.email)))
    encoder.encode()
  }
  
  private def handleRequest( request:HttpRequest ){

    val method = request.getMethod
    val queryStringDecoder = new QueryStringDecoder( request.getUri() )
    val content = request.getContent().toString(CharsetUtil.UTF_8)

    ( method, queryStringDecoder.getPath.split('/').tail ) match {

      case ( HttpMethod.GET, Array("api","user",userId) )  =>
        userRepositoryService.remote ! PullData( userId.toInt )

      case ( HttpMethod.POST, Array("api","user") ) =>
        val user = read[User](content)
        userRepositoryService.remote ! StoreData(user)

      case ( HttpMethod.POST, Array("api","login") ) =>
        val credentials = read[Credentials](content)
        userRepositoryService.remote ! AuthenticateUser(credentials)

      case ( HttpMethod.POST, Array("api","game") ) =>
        val createGame = read[RegisterGame](content)
        if( createGame.authentication_key == WEB_AUTHICATION_KEY ){
          val game: Game = Game(createGame.parameters)
          gameRepositoryService.remote ! StoreData( game)
          gameManagerService.remote ! InitGame (game)
        }
        else{
          sendResponse( None, HttpResponseStatus.UNAUTHORIZED )
        }

      case ( HttpMethod.GET, Array("admin","status") ) =>
        sendResponse(Some(Status("Web",34567)),HttpResponseStatus.OK)
    }
  }


  private def sendResponse( value:Option[AnyRef], status:HttpResponseStatus, headers : (String,String)* ){
    val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, status )

    value.foreach{
      data =>
      val str = write( data )
      response.setHeader(HttpHeaders.Names.CONTENT_TYPE, "application/json; charset=UTF-8")
      response.setContent(ChannelBuffers.copiedBuffer( str, CharsetUtil.UTF_8))
    }

    headers.foreach { header => response.setHeader(header._1,header._2) }

    val future = channel.write(response)
    future.addListener(ChannelFutureListener.CLOSE)
  }
}

@ChannelHandler.Sharable
class HttpRequestHandler(services : Services) extends SimpleChannelUpstreamHandler {

  override def messageReceived( ctx:ChannelHandlerContext , e:MessageEvent ) {
    ( new RequestActor(services) ).start() ! e
  }

  override def exceptionCaught( ctx:ChannelHandlerContext, e:ExceptionEvent ){
      e.getChannel.close
  }
}
