package org.psug.usi.service

import actors.{OutputChannel, Actor}
import collection.mutable.HashMap
import org.psug.usi.domain._
import org.psug.usi.store.StoreData

/**
 * User: alag
 * Date: 2/17/11
 * Time: 11:32 PM
 */

case class Register( userId:Int )

case class UserAnswer( userId:Int, questionIndex:Int, answerIndex:Int )

// Question send to the user => if we assume that we send this question to an actor that has a ref on user id, we should not need to have userId in this class
case class UserQuestion( userId:Int, question:Option[Question], score:Int, scoreSlice:Option[Array[UserScore]] = None )



object GameManagerTimer {
  case class QuestionTimeout(questionIndex:Int, timoutSec:Int)
}
class GameManagerTimer extends Actor {
  import GameManagerTimer._

  start
  def act {
    loop {
      react {
        case questionTimeout:QuestionTimeout =>
          Thread.sleep(questionTimeout.timoutSec*1000)
          sender ! questionTimeout
      }
    }
  }
}

/**
 * A game manager: handle question/anwser and timeout
 */
class GameManagerService( val game:Game, val gameUserHistoryRepositoryService:GameUserHistoryRepository with RepositoryService ) extends RemoteService {
  import GameManagerTimer._


  val scorer = new Scorer(game.numPlayer)
  val timer = new GameManagerTimer

  val players = new Array[Int]( game.numPlayer )
  var playerIndex = 0

  val playerActors = new HashMap[Int,OutputChannel[Any]]

  var currentQuestionIndex = -1 // start -1 cause proceedToNextQuestion do a +=1 -> 1st question case

  val playersHistory = new HashMap[Int,List[AnswerHistory]]

  def act {
    loop {
      react {
        case Register( userId ) => register( userId )
        case UserAnswer( userId, questionIndex, answerIndex ) if( questionIndex == currentQuestionIndex ) => answer( userId, answerIndex )
        case QuestionTimeout( questionIndex, timeoutSec ) if( questionIndex == currentQuestionIndex ) => timeout()
        case x => 
      }
    }
  }


  /*
   * Long polling stage -> all user register until the required game player is reach, proceedToNextQuestion is called
   */
  private def register( userId:Int ){
    players(playerIndex) = userId
    playerIndex += 1
    playerActors( userId ) =  sender
    if( playerIndex >= game.numPlayer ) proceedToNextQuestion()
  }

  /*
   * A user answer the current question, when all player respond or timeout occurs proceedToNextQuestion is called
   */
  def answer( userId:Int, answerIndex:Int ){
    val currentQuestion = game.questions(currentQuestionIndex)

    playersHistory( userId ) = AnswerHistory(currentQuestionIndex, answerIndex ) :: playersHistory.getOrElse( userId, Nil )

    val answerValue = if( currentQuestion.answers( answerIndex ).status ) currentQuestion.value else 0
    // TODO: should remove actor on scorer?
    scorer !? ScorerAnwserValue( userId, answerValue )

    playerActors( userId ) = sender
    if( playerActors.size >= game.numPlayer ){
      proceedToNextQuestion()
    }

  }

  // TODO: WARNING scorer is not used with message passing here => direct call for userScore & scoreSlice

  private def proceedToNextQuestion(){
    currentQuestionIndex += 1

    playerActors.foreach{
      case ( userId, playerActor ) =>
        if( currentQuestionIndex < game.questions.size ){
          // still remain some question
          playerActor ! UserQuestion( userId, Some( game.questions(currentQuestionIndex) ), scorer.userScore( userId ), None )
        }
        else{
          // game ended -> send score slice & save player history
          playerActor ! UserQuestion( userId, None, scorer.userScore( userId ), Some( scorer.scoreSlice( userId ) ) )
          // store data
          gameUserHistoryRepositoryService ! StoreData( GameUserHistory( GameUserKey( game.id, userId ), playersHistory.getOrElse( userId, Nil ) ) )
        }
        
    }
    playerActors.clear
    
    timer ! QuestionTimeout( currentQuestionIndex, game.timeoutSec )


  }


  private def timeout(){
    for( userId <- players ){
      if( !playerActors.contains( userId ) ){
        // TODO: should remove actor on scorer?
        scorer !? ScorerAnwserValue( userId, 0 )
      }
    }
    proceedToNextQuestion()
  }
}