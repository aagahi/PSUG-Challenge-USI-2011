package org.psug.usi.netty

import org.psug.usi.domain._
import org.jboss.netty.util.CharsetUtil
import net.liftweb.json.{Serialization, NoTypeHints}
import org.jboss.netty.channel._
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.handler.codec.http._
import net.liftweb.json.Serialization.{read, write}
import org.psug.usi.store.{StoreData, PullData, DataPulled, DataStored}
import org.psug.usi.akka.Receiver
import io.{Codec, Source}
import akka.util.Logging
import org.psug.usi.service._
import scala.collection.JavaConversions._
/**
 * User: alag
 * Date: 2/22/11
 * Time: 1:07 AM
 */

case class Status( nodeType:String )

class RequestActor(services : Services, webAuthenticationKey:String ) extends Receiver with Logging {

  import services._
  implicit val formats = Serialization.formats(NoTypeHints)

  var channel:Channel = null


  def receive = {
    case event:MessageEvent =>
      channel = event.getChannel
      event.getMessage match
      {
        case request:HttpRequest => handleRequest( request )
        case _ => log.warn( "Unknown message" )
      }

    case DataStored( Right( data ) )	=>  sendResponse( Some( data ), HttpResponseStatus.OK )
    case DataStored( Left( message ) )	=> log.debug(message); sendResponse( None, HttpResponseStatus.BAD_REQUEST )

    case DataPulled( Some( data ) )		=>  sendResponse( Some( data ), HttpResponseStatus.OK )
    case DataPulled( None )			=> sendResponse( None, HttpResponseStatus.BAD_REQUEST )

    case UserAuthenticated (Left(user)) =>    sendResponse( None, HttpResponseStatus.CREATED, (HttpHeaders.Names.SET_COOKIE, encodeUserAsCookie(user)))
    case UserAuthenticated (Right(message)) => sendResponse( Some(message), HttpResponseStatus.UNAUTHORIZED)

    case QuestionResponse( question ) => sendResponse( Some( question ), HttpResponseStatus.OK )

    case ScoreSliceUnavailable => sendResponse( None, HttpResponseStatus.BAD_REQUEST )
    case ScoreSlice(ranking) => sendResponse( Some( ranking ), HttpResponseStatus.OK )
  }

  private def encodeUserAsCookie(user : User) = {
    val encoder = new CookieEncoder(true)
    encoder.addCookie("session_key", AuthenticationToken.encrypt(AuthenticationToken(user.id,user.mail)))
    encoder.encode()
  }
  private def decodeCookieAsAuthenticationToken(cookie:String):Option[AuthenticationToken] = {
    val decoder = new CookieDecoder()
    val cookies = decoder.decode(cookie)
    cookies.find( _.getName == "session_key" ).map( cookie => AuthenticationToken.decrypt( cookie.getValue ) )
  }
  
  private def handleRequest( request:HttpRequest ){

    val method = request.getMethod
    val queryStringDecoder = new QueryStringDecoder( request.getUri() )
    val content = request.getContent().toString(CharsetUtil.UTF_8)
    val path= if (queryStringDecoder.getPath=="/") Nil else queryStringDecoder.getPath.split('/').tail.toList

    log.info( "Method: " + method +  " - Path:" + path.mkString(",") )


    ( method, path ) match {

      case ( HttpMethod.GET, "api"::"user"::userId::Nil )  =>
        userRepositoryService ! PullData( userId.toInt )

      case ( HttpMethod.POST, "api"::"user"::Nil ) =>
        val userVO = read[UserVO](content)
        userRepositoryService ! StoreData( User( userVO ) )

      case ( HttpMethod.POST, "api"::"login"::Nil ) =>
        val credentials = read[Credentials](content)
        userRepositoryService ! AuthenticateUser(credentials)

      case ( HttpMethod.POST, "api"::"game"::Nil ) =>
        val createGame = read[RegisterGame](content)
        if( createGame.authentication_key == webAuthenticationKey ){
          val game: Game = Game(createGame.parameters)
          gameRepositoryService ! StoreData( game )
          gameManagerService ! InitGame (game)
        }
        else{
          sendResponse( None, HttpResponseStatus.UNAUTHORIZED )
        }

      case ( HttpMethod.GET, "api"::"question"::questionIndex::Nil ) =>
        decodeCookieAsAuthenticationToken( request.getHeader( HttpHeaders.Names.SET_COOKIE ) ) match
        {
          case Some( AuthenticationToken( userId, mail ) ) =>
           // api assume question starts at 1 but gamemanager starts at 0
            gameManagerService ! QueryQuestion( userId, questionIndex.toInt-1 )

          case None => 
            sendResponse( None, HttpResponseStatus.UNAUTHORIZED )
        }




      case ( HttpMethod.GET, "api"::"score"::Nil ) =>
        // TODO: what if param is not provided
        if( queryStringDecoder.getParameters().get("authentication_key").get(0) == webAuthenticationKey ) {
          val mail = queryStringDecoder.getParameters().get("user_mail").get(0)
          gameManagerService ! QueryScoreSliceAudit (mail)
        }
        else{
          sendResponse( None, HttpResponseStatus.UNAUTHORIZED )
        }


      case ( HttpMethod.GET, "admin"::"status"::Nil ) =>
        sendResponse(Some(Status("Web")),HttpResponseStatus.OK)




      case ( HttpMethod.GET, "web"::Nil )  =>
        sendPage( "/web/index.html" )

      case ( HttpMethod.GET, "web"::subPath  )  =>
        sendPage( queryStringDecoder.getPath )

      case ( HttpMethod.GET, page::Nil )  =>
        sendRedirectToWeb(page)

      case _ => log.warn( "Unknown request ("+method+"): " + queryStringDecoder.getPath  )
    }
  }

  private def sendPage( path:String ){
    val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK )
    val contentType = path.substring( path.lastIndexOf(".") + 1 ) match {
      case "html" => "text/html; charset=utf-8"
      case "js" => "text/javascript; charset=utf-8"
      case x =>
        log.warn( "Unknown file type "+x+", using binary content type" )
        "application/binary"
    }
    response.setHeader(HttpHeaders.Names.CONTENT_TYPE, contentType )


    val content = Source.fromFile( "."+path)(Codec.UTF8).mkString
    response.setContent(ChannelBuffers.copiedBuffer( content, CharsetUtil.UTF_8))
    val future = channel.write(response)
    future.addListener(ChannelFutureListener.CLOSE)
  }

  def sendRedirectToWeb(page: String) = {
    val response = new DefaultHttpResponse(
      HttpVersion.HTTP_1_1,
      HttpResponseStatus.MOVED_PERMANENTLY)
    page match {
      case "" => response.setHeader(HttpHeaders.Names.LOCATION, "/web/index.html")
      case  _  => response.setHeader(HttpHeaders.Names.LOCATION, "/web/"+page)
    }
    val future = channel.write(response)
    future.addListener(ChannelFutureListener.CLOSE)
  }

  private def sendResponse( value:Option[AnyRef], status:HttpResponseStatus, headers : (String,String)* ){
    val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, status )

    value.foreach{
      data =>
      val str = write( data )
      response.setHeader(HttpHeaders.Names.CONTENT_TYPE, "application/json; charset=utf-8")
      response.setContent(ChannelBuffers.copiedBuffer( str, CharsetUtil.UTF_8))
    }

    headers.foreach { header => response.setHeader(header._1,header._2) }

    val future = channel.write(response)
    future.addListener(ChannelFutureListener.CLOSE)
  }
}

@ChannelHandler.Sharable
class HttpRequestHandler(services : Services, webAuthenticationKey:String ) extends SimpleChannelUpstreamHandler {

  override def messageReceived( ctx:ChannelHandlerContext , e:MessageEvent ) {
    ( new RequestActor(services, webAuthenticationKey) ).start() ! e
  }

  override def exceptionCaught( ctx:ChannelHandlerContext, e:ExceptionEvent ){
      e.getChannel.close
  }
}
