package org.psug.usi.netty

import org.psug.usi.domain._
import org.jboss.netty.util.CharsetUtil
import net.liftweb.json.{Serialization, NoTypeHints}
import org.jboss.netty.channel._
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.handler.codec.http._
import net.liftweb.json.Serialization.{read, write}
import org.psug.usi.store.{StoreData, PullData, DataPulled, DataStored}
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


class HttpOutput( channel:Channel ) extends Logging {
  implicit val formats = Serialization.formats(NoTypeHints)

  def sendPage( path:String ){
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

  def sendResponse( value:Option[AnyRef], status:HttpResponseStatus, headers : (String,String)* ){
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
class HttpRequestHandler(services : Services, webAuthenticationKey:String ) extends SimpleChannelUpstreamHandler with Logging {

  import services._
  implicit val formats = Serialization.formats(NoTypeHints)


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

  def handleRequest( out:HttpOutput, request:HttpRequest ){

    val method = request.getMethod
    val queryStringDecoder = new QueryStringDecoder( request.getUri() )
    // TODO: check the spec to see if content is supposed to be UTF-8 or ISO LATIN
    val content = request.getContent().toString(CharsetUtil.UTF_8)
    val path= if (queryStringDecoder.getPath=="/") Nil else queryStringDecoder.getPath.split('/').tail.toList

    log.info( "Method: " + method +  " - Path:" + path.mkString(",") )


    ( method, path ) match {

      case ( HttpMethod.GET, "api"::"user"::userId::Nil )  =>
        userRepositoryService.callback( PullData( userId.toInt ) ){
          case DataPulled( Some( data ) )	=>  out.sendResponse( Some( data ), HttpResponseStatus.OK )
          case DataPulled( None )	=> out.sendResponse( None, HttpResponseStatus.BAD_REQUEST )
        }


      case ( HttpMethod.POST, "api"::"user"::Nil ) =>
        val userVO = read[UserVO](content)
        userRepositoryService.callback( StoreData( User( userVO ) ) ){
          case DataStored( Right( data ) )	=>  out.sendResponse( Some( data ), HttpResponseStatus.OK )
          case DataStored( Left( message ) )	=> log.debug(message); out.sendResponse( None, HttpResponseStatus.BAD_REQUEST )
        }


      case ( HttpMethod.POST, "api"::"login"::Nil ) =>
        val credentials = read[Credentials](content)
        userRepositoryService.callback( AuthenticateUser(credentials) ){
          case UserAuthenticated (Left(user)) =>  out.sendResponse( None, HttpResponseStatus.CREATED, (HttpHeaders.Names.SET_COOKIE, encodeUserAsCookie(user)))
          case UserAuthenticated (Right(message)) => out.sendResponse( Some(message), HttpResponseStatus.UNAUTHORIZED)
        }

        
      case ( HttpMethod.POST, "api"::"game"::Nil ) =>
        val createGame = read[RegisterGame](content)
        if( createGame.authentication_key == webAuthenticationKey ){
          val game: Game = Game(createGame.parameters)

          gameRepositoryService.callback( StoreData( game ) ){
            case DataStored( Right( data ) )	=>
              gameManagerService ! InitGame (game)
              out.sendResponse( Some( data ), HttpResponseStatus.OK )
            case DataStored( Left( message ) )	=> log.debug(message); out.sendResponse( None, HttpResponseStatus.BAD_REQUEST )
          }

        }
        else{
          out.sendResponse( None, HttpResponseStatus.UNAUTHORIZED )
        }




      case ( HttpMethod.GET, "api"::"question"::questionIndex::Nil ) =>
        decodeCookieAsAuthenticationToken( request.getHeader( HttpHeaders.Names.SET_COOKIE ) ) match
        {
          case Some( AuthenticationToken( userId, mail ) ) =>
           // api assume question starts at 1 but gamemanager starts at 0
            gameManagerService.callback( QueryQuestion( userId, questionIndex.toInt-1 ) ){
              case QuestionResponse( question, score ) => out.sendResponse( Some( QuestionVO( question, score ) ), HttpResponseStatus.OK )
              case _ => out.sendResponse( None, HttpResponseStatus.BAD_REQUEST )
            }

          case None =>
            out.sendResponse( None, HttpResponseStatus.UNAUTHORIZED )
        }


      case ( HttpMethod.POST, "api"::"answer"::questionIndex::Nil ) =>
        decodeCookieAsAuthenticationToken( request.getHeader( HttpHeaders.Names.SET_COOKIE ) ) match
        {
          case Some( AuthenticationToken( userId, mail ) ) =>
            val answerVO = read[AnswerVO](content)

           // api assume question starts at 1 but gamemanager starts at 0
            gameManagerService.callback(  UserAnswer( userId, questionIndex.toInt-1, answerVO.answer ) ){
              case UserAnswerResponse( answerStatus, correctAnwser, score ) => out.sendResponse( Some( UserAnswerResponseVO( answerStatus, correctAnwser, score ) ), HttpResponseStatus.OK )
              case _ => out.sendResponse( None, HttpResponseStatus.BAD_REQUEST )
            }

          case None =>
            out.sendResponse( None, HttpResponseStatus.UNAUTHORIZED )
        }




      case ( HttpMethod.GET, "api"::"score"::Nil ) =>
        // TODO: what if param is not provided
        if( queryStringDecoder.getParameters().get("authentication_key").get(0) == webAuthenticationKey ) {
          val mail = queryStringDecoder.getParameters().get("user_mail").get(0)

          gameManagerService.callback( QueryScoreSliceAudit (mail) ){
            case ScoreSlice(ranking) => out.sendResponse( Some( ranking ), HttpResponseStatus.OK )
            case ScoreSliceUnavailable => out.sendResponse( None, HttpResponseStatus.BAD_REQUEST )
          }
        }
        else{
          out.sendResponse( None, HttpResponseStatus.UNAUTHORIZED )
        }


      case ( HttpMethod.GET, "api"::"audit"::Nil ) =>
        // TODO: what if param is not provided
        if( queryStringDecoder.getParameters().get("authentication_key").get(0) == webAuthenticationKey ) {
          val mail = queryStringDecoder.getParameters().get("user_mail").get(0)

          gameManagerService.callback( QueryHistory(mail,None) ){
            case GameAnwsersHistory( answers ) => out.sendResponse( Some( answers ), HttpResponseStatus.OK )
            case _ =>  out.sendResponse( None, HttpResponseStatus.BAD_REQUEST )
          }
        }
        else{
          out.sendResponse( None, HttpResponseStatus.UNAUTHORIZED )
        }


      case ( HttpMethod.GET, "api"::"audit"::questionIndex::Nil ) =>
        // TODO: what if param is not provided
        if( queryStringDecoder.getParameters().get("authentication_key").get(0) == webAuthenticationKey ) {
          val mail = queryStringDecoder.getParameters().get("user_mail").get(0)

           // api assume question starts at 1 but gamemanager starts at 0
          gameManagerService.callback( QueryHistory(mail, Some( questionIndex.toInt - 1 ) ) ){
            case GameAnwserHistory( answer ) => out.sendResponse( Some( answer ), HttpResponseStatus.OK )
            case _ =>  out.sendResponse( None, HttpResponseStatus.BAD_REQUEST )
          }
        }
        else{
          out.sendResponse( None, HttpResponseStatus.UNAUTHORIZED )
        }



      case ( HttpMethod.GET, "admin"::"status"::Nil ) =>
        out.sendResponse(Some(Status("Web")),HttpResponseStatus.OK)


      case ( HttpMethod.GET, "web"::Nil )  =>
        out.sendPage( "/web/index.html" )

      case ( HttpMethod.GET, "web"::subPath  )  =>
        out.sendPage( queryStringDecoder.getPath )

      case ( HttpMethod.GET, page::Nil )  =>
        out.sendRedirectToWeb(page)

      case _ =>
        log.warn( "Unknown request ("+method+"): " + queryStringDecoder.getPath  )
        out.sendResponse( None, HttpResponseStatus.BAD_REQUEST )
    }
  }


  

  override def messageReceived( ctx:ChannelHandlerContext , e:MessageEvent ) {

    e.getMessage match
      {
        case request:HttpRequest => handleRequest( new HttpOutput(ctx.getChannel), request )
        case x => log.warn( "Unknown message: "+ x  )
      }
  }

  override def exceptionCaught( ctx:ChannelHandlerContext, e:ExceptionEvent ){
      e.getChannel.close
  }
}
