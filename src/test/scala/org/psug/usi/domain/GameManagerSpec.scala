package org.psug.usi.domain

import org.specs._
import org.psug.usi.store._
import org.psug.usi.service._
import akka.dispatch.Futures
import akka.dispatch.Future
import org.psug.usi.utils.{UserGenerator, GamePlayer, GameGenerator}
import org.psug.usi.akka.Receiver

/**
 * User: alag
 * Date: 4/4/11
 * Time: 11:59 PM
 */

case object FireLastMessage
case object LastMessageFired
case object PullAllMessages

class TestGameManagerTimer extends GameManagerTimer {

  private var messages:List[TimeoutMessage] = Nil

  private var gameManagerActor:Receiver = null

  override def schedule( questionTimeout:TimeoutMessage, target:Receiver ){
    gameManagerActor = target
    synchronized{ messages = questionTimeout :: messages }

  }

  def fireLastMessage(){
    synchronized{
      gameManagerActor ! messages.head
      messages = messages.tail
    }
  }

  def awaitOneOrMoreMessage = {
    while( synchronized{ messages.size == 0 } ) Thread.sleep(10)
    synchronized{ messages }
  }

}
class GameManagerSpec  extends SpecificationWithJUnit {

  val serverServices = new ServerServices
  val services = new ClientServices()
  import services._




  def startRepository:Unit = {
    serverServices.launch
    userRepositoryService !? ClearRepository
    gameRepositoryService !? ClearRepository

  }

  def exitRepository = {
    userRepositoryService !? ClearRepository
    gameRepositoryService !? ClearRepository
    serverServices.shutdown
  }


  

  "game manager" should {

    startRepository.before
    exitRepository.after



    "register all players, provide question, userScore each answer, save user history after last response, and provide userScore slice (no timeout scenario)" in {

      val game = GameGenerator( 3, 4, 160 )
      val users = UserGenerator( userRepositoryService, 160 )


      gameManagerService !? InitGame(game)

      val gamePlayer = new GamePlayer( gameManagerService, game, users )

      var currentQuestion = 0

      // Register
      users.map( user => gameManagerService !? Register( user ) )

      // Ask for Q1
      val futuresQ1 = users.map( user => (gameManagerService !! QueryQuestion( user.id, currentQuestion )).asInstanceOf[Future[QuestionResponse]] )
      Futures.awaitAll( futuresQ1 )
      futuresQ1.map( _.result ).foreach{
        case Some( QuestionResponse( nextQuestion, score ) )=>
          nextQuestion must be_==( game.questions(currentQuestion ) )
          score must be_==( 0 )
        case _ => fail
      }

      // Answser Q1
      users.foreach{
        user =>
          val UserAnswerResponse( answerStatus, correctAnwser, score ) = (gameManagerService !? UserAnswer( user.id, currentQuestion, gamePlayer.answer( user, currentQuestion ) ) ).asInstanceOf[UserAnswerResponse]
          score must be_== ( gamePlayer.expectedScore( user, currentQuestion ) )
      }

      currentQuestion += 1
      // Ask for Q2
      val futuresQ2 = users.map( user => ( gameManagerService !! QueryQuestion( user.id, currentQuestion ) ).asInstanceOf[Future[QuestionResponse]] )
      Futures.awaitAll( futuresQ2 )
      futuresQ2.map( _.result ).foreach{
        case Some( QuestionResponse( nextQuestion, score ) )=>
          nextQuestion must be_==( game.questions( currentQuestion ) )
        case _ => fail
      }

      // Answser Q2
      users.foreach{
        user =>
          val UserAnswerResponse( answerStatus, correctAnwser, score ) = (gameManagerService !? UserAnswer( user.id, currentQuestion, gamePlayer.answer( user, currentQuestion ) ) ).asInstanceOf[UserAnswerResponse]
          score must be_== ( gamePlayer.expectedScore( user, currentQuestion ) )
      }

      currentQuestion += 1
      // Ask for Q3
      val futuresQ3 = users.map( user => ( gameManagerService !! QueryQuestion( user.id, currentQuestion ) ).asInstanceOf[Future[QuestionResponse]] )
      Futures.awaitAll( futuresQ3 )
      futuresQ3.map( _.result ).foreach{
        case Some( QuestionResponse( nextQuestion, score ) )=>
          nextQuestion must be_==( game.questions( currentQuestion ) )
        case _ => fail
      }

      // Answser Q3
      users.foreach{
        user =>
          val UserAnswerResponse( answerStatus, correctAnwser, score ) = (gameManagerService !? UserAnswer( user.id, currentQuestion, gamePlayer.answer( user, currentQuestion ) ) ).asInstanceOf[UserAnswerResponse]
          score must be_== ( gamePlayer.expectedScore( user, currentQuestion ) )
      }

      // Get userScore slices
      users.foreach{
        user =>
          val ScoreSlice( ranking ) = (gameManagerService !? QueryScoreSlice( user.id ) ).asInstanceOf[ScoreSlice]
          val ScoreSlice( rankingAudit ) = (gameManagerService !? QueryScoreSliceAudit( user.mail ) ).asInstanceOf[ScoreSlice]

          ranking.deepEquals( rankingAudit ) must beTrue
          ranking.deepEquals( gamePlayer.expectedScoreSlice(user) ) must beTrue
      }

      // Check history
      users.foreach{
        user =>
        val DataPulled( Some( userHistory ) ) = serverServices.gameUserHistoryService !? PullData( GameUserKey( game.id, user.id ) )
        val expectedHistory = game.questions.zipWithIndex.reverse.map{ case( q, i ) => AnswerHistory( i, gamePlayer.answer( user, i ) ) }
        userHistory.asInstanceOf[GameUserHistory].anwsers must be_==( expectedHistory )
      }

    }


    "register all players, provide question, userScore each answer, save user history after last response, and provide userScore slice (timeout scenario)" in {
      val game = GameGenerator( 3, 4, 160 )
      val users = UserGenerator( userRepositoryService, 160 )


      val timer = new TestGameManagerTimer
      val gameManager = new GameManager( gameUserHistoryService, serverServices.userRepositoryService, timer )
      gameManager.start
      gameManager !? InitGame (game)
      val gamePlayer = new GamePlayer( gameManagerService, game, users )

      var currentQuestion = 0

      // Register
      users.map{ user => gameManager !? Register( user ) }
      
      var messages = timer.awaitOneOrMoreMessage
      messages.size must be_==( 1 )
      messages.head must be_==( TimeoutMessage( TimeoutType.LOGIN, currentQuestion, game.loginTimeoutSec ) )


      // 50% user ask for Q1 => we should get a Login timeout
      val futuresQ1 = for( user <- users ; if( user.id%2 == 0 ) )
        yield (gameManager !! QueryQuestion( user.id, currentQuestion )).asInstanceOf[Future[QuestionResponse]]


      futuresQ1.forall( ! _.isCompleted ) must beTrue

      // fire login
      timer.fireLastMessage()

      Futures.awaitAll( futuresQ1 )
      val futuresQ1Results = futuresQ1.map( _.result )
      futuresQ1Results.size must be_==( users.size/2 )
      futuresQ1Results.foreach{
        case Some( QuestionResponse( nextQuestion, score ) )=>
          score must be_==( 0 )
          nextQuestion must be_==( game.questions(currentQuestion ) )
        case _ => fail
      }

      messages = timer.awaitOneOrMoreMessage
      messages.size must be_==( 1 )
      messages.head must be_==( TimeoutMessage( TimeoutType.QUESTION, currentQuestion, game.questionTimeFrameSec ) )


      // 25% user answers to Q1
      val answerQ1Users = for( user <- users ; if( user.id%4 == 0 ) ) yield {
        val UserAnswerResponse( answerStatus, correctAnwser, score ) = (gameManager !? UserAnswer( user.id, currentQuestion, gamePlayer.answer( user, currentQuestion ) ) ).asInstanceOf[UserAnswerResponse]
        score must be_== ( gamePlayer.expectedScore( user, currentQuestion ) )
        user
      }


      // fire end question
      timer.fireLastMessage()
      
      currentQuestion += 1

      messages = timer.awaitOneOrMoreMessage
      messages.size must be_==( 1 )
      messages.head must be_==( TimeoutMessage( TimeoutType.SYNCRO, currentQuestion, game.synchroTimeSec ) )



      // 25% user ask for Q2 => we should get a question + synchro timeout
      val futuresQ2 = answerQ1Users.map( user => (gameManager !! QueryQuestion( user.id, currentQuestion )).asInstanceOf[Future[QuestionResponse]] )


      futuresQ2.forall( ! _.isCompleted ) must beTrue

      // fire end syncho
      timer.fireLastMessage()


      messages = timer.awaitOneOrMoreMessage
      messages.size must be_==( 1 )
      messages.head must be_==( TimeoutMessage( TimeoutType.QUESTION, currentQuestion, game.questionTimeFrameSec ) )


      Futures.awaitAll( futuresQ2 )
      futuresQ2.map( _.result ).foreach{
        case Some( QuestionResponse( nextQuestion, score ) )=>
          nextQuestion must be_==( game.questions( currentQuestion ) )
        case _ => fail
      }

      gameManager.stop

    }

  }

}