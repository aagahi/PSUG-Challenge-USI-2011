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

  def sendPage( path:String , request:HttpRequest){

    val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK )
    val contentType = path.substring( path.lastIndexOf(".") + 1 ) match {
      case "html" => "text/html; charset=utf-8"
      case "js" => "text/javascript; charset=utf-8"
      case "css" => "text/css"
      case "png" => "image/png"
      case "jpg" => "image/jpeg"
      case x =>
        log.warn( "Unknown file type "+x+", using binary content type" )
        "application/binary"
    }
    response.setHeader(HttpHeaders.Names.CONTENT_TYPE, contentType )

    // Hard coded
    val existingLanguages=List("fr")
    val defaultLanguage="fr"

    val acceptedLangs=request.getHeader("Accept-Language") match {
      case l if (l!=null) => l.split("[,;]").toList
      case _ => List("fr")
    }

    val lang=acceptedLangs.filter(existingLanguages.contains(_) ) match {
      case Nil   => defaultLanguage
      case l => l.head
    }
    val specificPath=path.replaceAll("web/", "web/"+lang+"/")
    val content = Source.fromFile( "."+specificPath)(Codec.UTF8).mkString
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
      response.setContent( ChannelBuffers.copiedBuffer( str, CharsetUtil.UTF_8) )
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
  private def decodeCookieAsAuthenticationToken(request:HttpRequest):Option[AuthenticationToken] = {
    val cookieStr = request.getHeader( HttpHeaders.Names.COOKIE )
    val decoder = new CookieDecoder()
    val cookies = decoder.decode(cookieStr)
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
          case DataStored( Right( data ) )	=> out.sendResponse( None, HttpResponseStatus.CREATED )
          case DataStored( Left( message ) )	=> log.debug(message); out.sendResponse( None, HttpResponseStatus.BAD_REQUEST )
        }


      case ( HttpMethod.POST, "api"::"login"::Nil ) =>
        val credentials = read[Credentials](content)
        userRepositoryService.callback( AuthenticateUser(credentials) ){
          case UserAuthenticated (Left(user)) =>
            gameManagerService.callback( Register( user ) ){
              case RegisterSuccess => out.sendResponse( None, HttpResponseStatus.CREATED, (HttpHeaders.Names.SET_COOKIE, encodeUserAsCookie(user)))
              case _ => out.sendResponse( None, HttpResponseStatus.BAD_REQUEST )
            }


          case UserAuthenticated (Right(message)) => out.sendResponse( Some(message), HttpResponseStatus.UNAUTHORIZED)
        }

        
      case ( HttpMethod.POST, "api"::"game"::Nil ) =>

        val createGame = read[RegisterGame](content)
        if( createGame.authentication_key == webAuthenticationKey ){
          val game: Game = Game(createGame.parameters)
          gameRepositoryService.callback( StoreData( game ) ){
            case DataStored( Right( data ) )	=>
              gameManagerService.callback( InitGame (game) ){
                case InitGameSuccess => log.info( "Game "+game.id+" initialized")
              }
              out.sendResponse( None, HttpResponseStatus.CREATED )
            case DataStored( Left( message ) )	=> log.debug(message); out.sendResponse( None, HttpResponseStatus.BAD_REQUEST )
          }

        }
        else{
          out.sendResponse( None, HttpResponseStatus.UNAUTHORIZED )
        }




      case ( HttpMethod.GET, "api"::"question"::questionIndex::Nil ) =>
        decodeCookieAsAuthenticationToken( request ) match
        {
          case Some( AuthenticationToken( userId, mail ) ) =>
            log.info("Get Q " + questionIndex )
           // api assume question starts at 1 but gamemanager starts at 0
            gameManagerService.callback( QueryQuestion( userId, questionIndex.toInt-1 ) ){
              case QuestionResponse( question, score ) =>
                log.info("Reply Q " + questionIndex )
                out.sendResponse( Some( QuestionVO( question, score ) ), HttpResponseStatus.OK )
              case _ => out.sendResponse( None, HttpResponseStatus.BAD_REQUEST )
            }

          case None =>
            log.info("Unable to get session cookie")
            out.sendResponse( None, HttpResponseStatus.UNAUTHORIZED )
        }


      case ( HttpMethod.POST, "api"::"answer"::questionIndex::Nil ) =>
        decodeCookieAsAuthenticationToken( request ) match
        {
          case Some( AuthenticationToken( userId, mail ) ) =>
            val answerVO = read[AnswerVO](content)

           // api assume question & anwser starts at 1 but gamemanager starts at 0
            gameManagerService.callback(  UserAnswer( userId, questionIndex.toInt-1, answerVO.answer-1 ) ){
              case UserAnswerResponse( answerStatus, correctAnwser, score ) => out.sendResponse( Some( UserAnswerResponseVO( answerStatus, correctAnwser, score ) ), HttpResponseStatus.OK )
              case _ => out.sendResponse( None, HttpResponseStatus.BAD_REQUEST )
            }

          case None =>
            out.sendResponse( None, HttpResponseStatus.UNAUTHORIZED )
        }


      // TODO: refactor / factorize audit code + load last game created for game repo in case of jvm restart


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
        out.sendPage( "/web/index.html", request )

      case ( HttpMethod.GET, List() )  =>
        out.sendRedirectToWeb("index.html")

      case ( HttpMethod.GET, "web"::subPath  )  =>
        out.sendPage( queryStringDecoder.getPath , request)

      case ( HttpMethod.GET, page::Nil )  =>
        out.sendRedirectToWeb(page)

      case _ =>
        log.warn( "Unknown request ("+method+"): " + path  )
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
