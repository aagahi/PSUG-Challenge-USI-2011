package org.psug.usi.games

/**
 * User: alag
 * Date: 2/17/11
 * Time: 12:19 AM
 */


import org.specs.runner.JUnit4
import org.specs._
import java.util.concurrent.atomic.AtomicInteger

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
}

