package org.psug.usi.rest

/**
 * User: alag
 * Date: 4/7/11
 * Time: 10:31 AM
 */

import org.psug.usi.domain._
import org.psug.usi.service._
import org.psug.usi.store._
import org.specs._
import org.psug.usi.netty.WebServer
import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner
import com.sun.jersey.api.client._
import net.liftweb.json.NoTypeHints
import net.liftweb.json.Serialization
import org.jboss.netty.handler.codec.http.CookieEncoder
import akka.dispatch.{Future, Futures}
import org.psug.usi.utils.{GamePlayer, AsyncExecutor, GameGenerator, UserGenerator}


@RunWith(classOf[JUnitSuiteRunner])
class GamePlaySpec extends SpecificationWithJUnit {

  implicit val formats = Serialization.formats(NoTypeHints)

  val serverServices = new SimpleRepositoryServices
  val services = new ClientServices()
  import services._


  def startRepository:Unit = {
    serverServices.launch
    webServer.start
    userRepositoryService !? ClearRepository
    gameRepositoryService !? ClearRepository
  }

  def exitRepository = {
    userRepositoryService !? ClearRepository
    gameRepositoryService !? ClearRepository
    webServer.stop
    serverServices.shutdown
  }


  setSequential()


  val webAuthenticationKey = "dummy"
  val listenPort = 12345
  val webServer : WebServer = new WebServer( listenPort, services, webAuthenticationKey )

  private[this] def webResource( path:String ) = new Client().resource("http://localhost:"+listenPort+path)

  // GET /api/question/N
  def getQuestionN( user:User, questionIndex:Int ):String = {
    val cookieEncoder = new CookieEncoder( false )
    cookieEncoder.addCookie("session_key",AuthenticationToken( user.id, user.mail ))
    webResource("/api/question/"+questionIndex).header("Cookie", cookieEncoder.encode() ).get(classOf[String] )
  }

  // POST /api/answer/N
  def postAnswerN( user:User, questionIndex:Int, anwser:AnswerVO ):(User,String) = {
    val cookieEncoder = new CookieEncoder( false )
    cookieEncoder.addCookie("session_key",AuthenticationToken( user.id, user.mail ))
    (user, webResource("/api/answer/"+questionIndex).header("Cookie", cookieEncoder.encode() ).post(classOf[String], Serialization.write(anwser) ))
  }


  "query question" should {

    startRepository.before
    exitRepository.after

    "return error on number 0 " in {
    }

    "return json when request question 1" in {
      val game = GameGenerator( 3, 4, 160 )
      val users = UserGenerator( userRepositoryService, 160 )
      gameManagerService !? InitGame(game)
      users.foreach( user => gameManagerService !? Register( user ) )

      val currentQuestion = 0
      val futures = users.map{
        user => AsyncExecutor().execute{ getQuestionN( user, currentQuestion+1 ) }
      }

      Futures.awaitAll( futures )
      futures.map( _.result ).foreach{
        case Some( str )=>
          val questionVO = Serialization.read[QuestionVO]( str )
          questionVO.question must be_==( game.questions( currentQuestion ).question )
          questionVO.answer_1 must be_==( game.questions( currentQuestion ).answers(0).anwser )
          questionVO.answer_2 must be_==( game.questions( currentQuestion ).answers(1).anwser )
          questionVO.answer_3 must be_==( game.questions( currentQuestion ).answers(2).anwser )
          questionVO.answer_4 must be_==( game.questions( currentQuestion ).answers(3).anwser )
          questionVO.score must be_==( 0 )
        case _ => fail
      }
    }

/*
    "not be available as long as query 1 + anwser has not been done" in {
    }


    "fail with POST" in {
    }

    "fail without a question number" in {
    }

    "fail without user cookie" in {
    }

    "fail with wrong/unkonwn user cookie" in {
    }
    */
      
    

  }

  "answer to question" should {
    startRepository.before
    exitRepository.after

    "return json if answed to appropriate question" in {
      val game = GameGenerator( 3, 4, 16 )
      val users = UserGenerator( userRepositoryService, 16 )
      gameManagerService !? InitGame(game)
      users.foreach( user => gameManagerService !? Register( user ) )
      val gamePlayer = new GamePlayer( gameManagerService, game, users )

      val currentQuestion = 0

      val futuresQ1 = users.map( user => (gameManagerService !! QueryQuestion( user.id, currentQuestion )).asInstanceOf[Future[QuestionResponse]] )
      Futures.awaitAll( futuresQ1 )



      val futures = users.map{
        // +1 on current question (we assume it start at 1)
        user => AsyncExecutor().execute{ postAnswerN( user, currentQuestion+1, AnswerVO( gamePlayer.answer( user, currentQuestion ) +1 ) ) }
      }

      Futures.awaitAll( futures )
      futures.map( _.result ).foreach{
        case Some( (user, str ) )=>
          val userAnswerResponseVO = Serialization.read[UserAnswerResponseVO]( str )

          gamePlayer.correctAnwser( user, currentQuestion ) must be_==( userAnswerResponseVO.are_u_right )
          game.correctAnswer( currentQuestion ) must be_==( userAnswerResponseVO.good_answer )
          gamePlayer.expectedScore( user, currentQuestion ) must be_==( userAnswerResponseVO.score )
        case _ => fail
      }


    }
  }


} 