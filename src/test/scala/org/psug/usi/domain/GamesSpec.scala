package org.psug.usi.domain

/**
 * User: alag
 * Date: 2/17/11
 * Time: 12:19 AM
 */

import org.specs._
import org.psug.usi.store._
import org.psug.usi.service._
import actors.{OutputChannel, Actor, Futures}
import scala.io.Source






case object FireLastMessage
case object LastMessageFired
case object PullAllMessages

class TestGameManagerTimer extends GameManagerTimer {

  private var messages:List[TimeoutMessage] = Nil

  private var gameManagerActor:OutputChannel[Any] = null

  override def handleQuestionTimeout( questionTimeout:TimeoutMessage ){
    gameManagerActor = sender
    messages = questionTimeout :: messages
  }

  override def handleOtherMessage( message:Any ){
    message match {

      case FireLastMessage =>
        gameManagerActor ! messages.head
        messages = messages.tail
        sender ! LastMessageFired

      case PullAllMessages =>
        sender ! messages
    }

  }

  def awaitOneOrMoreMessage = {
    var m:List[TimeoutMessage] = Nil
    while( m.size == 0 ) m = (this !? PullAllMessages ).asInstanceOf[List[TimeoutMessage]]
    m
  }

}

class GamesSpec extends SpecificationWithJUnit {

  val repositories = new SimpleRepositoryServices

  def startRepository = {
    repositories.start
  }

  def exitRepository = {
    repositories.gameRepositoryService.remote ! ClearRepository
    repositories.stop
  }

  "a game" should {
    "be definable using xml string" in {
      val game = Game( Source.fromFile( "./test-data/simplegamesession.xml" ).mkString )
      game.loginTimeoutSec must be_==( 3 )
      game.synchroTimeSec must be_==( 5 )
      game.questionTimeFrameSec must be_==( 11 )
      game.nbQuestions must be_==( 6 )
      game.nbUsersThreshold must be_==( 7 )
      game.flushUserTable must be_==( true )

      game.questions.size must be_==( 6 )
      0 to 4 foreach{ i => game.questions(i).value must be_==( 1 ) }
      game.questions( 5 ).value must be_==( 5 )

      game.questions.zipWithIndex.foreach{
        case( question, questionIndex ) =>
        question.question must be_==( "Q"+(questionIndex+1) )
        question.answers.zipWithIndex.foreach{
          case( answer, answserIndex ) =>
          answer.anwser must be_==( "A"+(questionIndex+1)+(answserIndex+1) )
          answer.status must be_==( questionIndex % 4 == answserIndex )  
        }
      }


    }
  }

  "in-memory game repository" should {
    import repositories._

    startRepository.before
    exitRepository.after

    val game = Game( questions = Question( "Q1", Answer( "A1", false )::Answer("A2", false)::Nil, 1 ) :: Nil, nbQuestions = 1 )

    "assign unique id to user when registering" in {
      val DataStored( Right( gameStored ) ) = gameRepositoryService.remote !? StoreData(game)
      gameStored.asInstanceOf[Game].id must be_!=( game.id )

    }

    "lookup game by id" in {

      val DataStored( Right( gameStored ) ) = gameRepositoryService.remote !? StoreData(game)
      val DataPulled( Some( gameFound ) ) = gameRepositoryService.remote !? PullData(gameStored.asInstanceOf[Game].id)
      gameFound.asInstanceOf[Game].questions.head.question must be_==( game.questions.head.question )

    }
  }

  "game manager" should {
    import repositories._

    startRepository.before
    exitRepository.after

    val game = Game( questions = Question( "Q1", Answer( "A11", false )::Answer("A12", true)::Nil, 1 )
                    :: Question( "Q2", Answer( "A21", false )::Answer("A22", true)::Nil, 2 )
                    :: Question( "Q3", Answer( "A31", false )::Answer("A32", true)::Nil, 3 )
                    :: Nil,
                    loginTimeoutSec = 5,
                    synchroTimeSec = 7,
                    questionTimeFrameSec = 11,
                    nbQuestions = 3,
                    flushUserTable = false,
                    nbUsersThreshold = 160 )

    val users = for( i <- 0 until game.nbUsersThreshold ) yield User( i, "firstName"+i, "lastName"+i, "email"+i, "password"+i )


    "register all players, provide question, score each answer, save user history after last response, and provide score slice (no timeout scenario)" in {

      val gameManager = new GameManagerService( repositories.gameUserHistoryService )
      gameManager.go
      gameManager ! InitGame(game)

      var currentQuestion = 0

      // Register
      users.map( user => gameManager.remote ! Register( user.id ) )

      // Ask for Q1
      val futuresQ1 = users.map( user => gameManager.remote !! QueryQuestion( user.id, currentQuestion ) )
      Futures.awaitAll( 5000, futuresQ1.toSeq:_* ).foreach{
        case Some( QuestionResponse( nextQuestion ) )=>
          nextQuestion must be_==( game.questions(currentQuestion ) )
        case _ => fail
      }

      // Answser Q1
      users.foreach{
        user =>
          val UserAnswerResponse( answerStatus, score ) = (gameManager.remote !? UserAnswer( user.id, currentQuestion, user.id%(game.questions(currentQuestion).answers.size) ) ).asInstanceOf[UserAnswerResponse]
          val expectedScore = if( answerStatus ) game.questions(currentQuestion).value else 0
          score must be_== ( expectedScore )
      }

      currentQuestion += 1
      // Ask for Q2
      val futuresQ2 = users.map( user => gameManager.remote !! QueryQuestion( user.id, currentQuestion ) )
      Futures.awaitAll( 5000, futuresQ2.toSeq:_* ).foreach{
        case Some( QuestionResponse( nextQuestion ) )=>
          nextQuestion must be_==( game.questions( currentQuestion ) )
        case _ => fail
      }

      // Answser Q2
      users.foreach{
        user =>
          val UserAnswerResponse( answerStatus, score ) = (gameManager.remote !? UserAnswer( user.id, currentQuestion, user.id%(game.questions(currentQuestion).answers.size) ) ).asInstanceOf[UserAnswerResponse]

          val expectedPrevScoreWithBonus = if( game.questions(currentQuestion).answers( user.id%(game.questions(currentQuestion-1).answers.size) ).status  ) game.questions(currentQuestion-1).value+1 else 0
          val expectedScore = if( answerStatus ) game.questions(currentQuestion).value+expectedPrevScoreWithBonus else expectedPrevScoreWithBonus
          score must be_== ( expectedScore )
      }

      currentQuestion += 1
      // Ask for Q3
      val futuresQ3 = users.map( user => gameManager.remote !! QueryQuestion( user.id, currentQuestion ) )
      Futures.awaitAll( 5000, futuresQ3.toSeq:_* ).foreach{
        case Some( QuestionResponse( nextQuestion ) )=>
          nextQuestion must be_==( game.questions( currentQuestion ) )
        case _ => fail
      }

      // Answser Q3
      users.foreach{
        user =>
          val UserAnswerResponse( answerStatus, score ) = (gameManager.remote !? UserAnswer( user.id, currentQuestion, user.id%(game.questions(currentQuestion).answers.size) ) ).asInstanceOf[UserAnswerResponse]
          val expectedScore = if( answerStatus ) (1+1 +2+1 +3+1) else 0
          score must be_== ( expectedScore )
      }

      // Get score slices
      users.foreach{
        user =>
          val scoreSlice = (gameManager.remote !? QueryScoreSlice( user.id ) ).asInstanceOf[Array[UserScore]]
          val minSliceSize = math.min( math.abs( gameManager.scorer.sliceRange.head ), gameManager.scorer.sliceRange.last )
          scoreSlice.size must be_>=( minSliceSize )
          scoreSlice.size must be_<( gameManager.scorer.sliceRange.size )
      }

      // Check history
      users.foreach{
        user =>
        val DataPulled( Some( userHistory ) ) = repositories.gameUserHistoryService !? PullData( GameUserKey( game.id, user.id ) )
        val expectedHistory = game.questions.zipWithIndex.reverse.map{ case( q, i ) => AnswerHistory( i, user.id%(q.answers.size) ) }
        userHistory.asInstanceOf[GameUserHistory].anwsers must be_==( expectedHistory )
      }

      gameManager ! Exit
    }


    "register all players, provide question, score each answer, save user history after last response, and provide score slice (timeout scenario)" in {

      val timer = new TestGameManagerTimer
      val gameManager = new GameManagerService( gameUserHistoryService, timer )
      gameManager.go
      gameManager ! InitGame (game)

      var currentQuestion = 0

      // Register
      users.map( user => gameManager ! Register( user.id ) )
      var messages = timer.awaitOneOrMoreMessage
      messages.size must be_==( 1 )
      messages.head must be_==( TimeoutMessage( TimeoutType.LOGIN, currentQuestion, game.loginTimeoutSec ) )


      // 50% user ask for Q1 => we should get a Login timeout
      val futuresQ1 = for( user <- users ; if( user.id%2 == 0 ) )
        yield gameManager !! QueryQuestion( user.id, currentQuestion )


      // not sure about it, but it seems that future msg passing is not deterministic so we wait for gamemanager to be in proper state
      while( ( gameManager !? QueryStats ) != ( GameManagerStats( users.size, users.size/2 ) ) ) Thread.sleep(10)

      futuresQ1.forall( ! _.isSet ) must beTrue

      // fire login
      timer !? FireLastMessage

      val futuresQ1Results = Futures.awaitAll( 5000, futuresQ1.toSeq:_* )
      futuresQ1Results.size must be_==( users.size/2 )
      futuresQ1Results.foreach{
        case Some( QuestionResponse( nextQuestion ) )=>
          nextQuestion must be_==( game.questions(currentQuestion ) )
        case _ => fail
      }

      messages = timer.awaitOneOrMoreMessage
      messages.size must be_==( 1 )
      messages.head must be_==( TimeoutMessage( TimeoutType.QUESTION, currentQuestion, game.questionTimeFrameSec ) )


      // 25% user answers to Q1
      val answerQ1Users = for( user <- users ; if( user.id%4 == 0 ) ) yield {
        val UserAnswerResponse( answerStatus, score ) = (gameManager !? UserAnswer( user.id, currentQuestion, user.id%(game.questions(currentQuestion).answers.size) ) ).asInstanceOf[UserAnswerResponse]
        val expectedScore = if( answerStatus ) game.questions(currentQuestion).value else 0
        score must be_== ( expectedScore )
        user
      }

      // fire end question
      timer !? FireLastMessage

      currentQuestion += 1

      messages = timer.awaitOneOrMoreMessage
      messages.size must be_==( 1 )
      messages.head must be_==( TimeoutMessage( TimeoutType.SYNCRO, currentQuestion, game.synchroTimeSec ) )



      // 25% user ask for Q2 => we should get a question + synchro timeout
      val futuresQ2 = answerQ1Users.map( user => gameManager !! QueryQuestion( user.id, currentQuestion ) )

      // not sure about it, but it seems that future msg passing is not deterministic so we wait for gamemanager to be in proper state
      while( ( gameManager !? QueryStats ) != ( GameManagerStats( users.size, answerQ1Users.size ) ) ) Thread.sleep(10)
      
      futuresQ2.forall( ! _.isSet ) must beTrue

      // fire end syncho
      timer !? FireLastMessage


      messages = timer.awaitOneOrMoreMessage
      messages.size must be_==( 1 )
      messages.head must be_==( TimeoutMessage( TimeoutType.QUESTION, currentQuestion, game.questionTimeFrameSec ) )


      val futuresQ2Results = Futures.awaitAll( 5000, futuresQ2.toSeq:_* )
      futuresQ2Results.foreach{
        case Some( QuestionResponse( nextQuestion ) )=>
          nextQuestion must be_==( game.questions( currentQuestion ) )
        case _ => fail
      }

      gameManager ! Exit

    }

  }
  
}

