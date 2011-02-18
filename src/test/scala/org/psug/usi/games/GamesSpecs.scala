package org.psug.usi.games

/**
 * User: alag
 * Date: 2/17/11
 * Time: 12:19 AM
 */

import org.specs.runner.JUnit4
import actors.Actor._
import org.specs._
import java.util.concurrent.atomic.AtomicInteger
import org.psug.usi.users.User


class GamesSpec extends SpecificationWithJUnit {

  def clearRepository = InMemoryGameRepository.reset

  "in-memory game repository" should { clearRepository.before

     
    val game = Game( questions = Question( "Q1", Answer( "A1", false )::Answer("A2", false)::Nil ) :: Nil )

    "assign unique id to user when registering" in {
      val counter = new AtomicInteger(0)
      InMemoryGameRepository.store(game){ case Right( game ) => game.id must be_!=("") ; counter.incrementAndGet }
      while( counter.get < 1 ) Thread.sleep(10)
    }


    "lookup game by id" in {
      val counter = new AtomicInteger(0)

      InMemoryGameRepository.store(game){
        game =>

        InMemoryGameRepository.findByStoreKey( game.right.get.id ){
          game => game.get.questions.head.question must be_==( "Q1" ); counter.incrementAndGet
        }
      }

      while( counter.get < 1 ) Thread.sleep(10)

    }
  }


  "game manager" should { clearRepository.before

    val game = Game( questions = Question( "Q1", Answer( "A11", false )::Answer("A12", false)::Nil )
                                  :: Question( "Q2", Answer( "A21", false )::Answer("A22", false)::Nil )
                                  :: Nil,
                     timeoutSec = 10,
                     numPlayer = 1000 )

    val users = for( i <- 0 until game.numPlayer ) yield User( i, "firstName"+i, "lastName"+i, "email"+i, "password"+i )


    "wait for game numPlayer and send questions (and scoreSlice after 1st answer)" in {
      var currentQuestion = 0

      val playerAckCount = new AtomicInteger(0)

      val endpoint = actor {
        loop {
          react {
            case UserQuestion( userId, Some( question ), scoreSlice ) =>
              question must be_==( game.questions(currentQuestion) )
              if( currentQuestion > 0 ){
                val Some( userScores ) = scoreSlice
                userScores.find( _.userId == userId ) must notBe( None )
              }
              playerAckCount.incrementAndGet
            case _ => fail("Unexpected message => must alway have a question")
          }
        }
      }

      val gameManager = new GameManager( game, endpoint )

      // 1st question
      users.foreach( userId => gameManager.send( userId, endpoint ) )
      while( playerAckCount.get < game.numPlayer ) Thread.sleep(10)


      // 2nd question
      playerAckCount.set(0)
      currentQuestion += 1
      users.foreach( user => gameManager.send( UserAnswer( user.id, currentQuestion, user.id%2 ), endpoint ) )


    }

  }
}

