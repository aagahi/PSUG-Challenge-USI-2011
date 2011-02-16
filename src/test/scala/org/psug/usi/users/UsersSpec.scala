package org.psug.usi.users

import org.specs.runner.JUnit4
import org.specs._
import java.util.concurrent.atomic.AtomicInteger

class UsersSpec extends SpecificationWithJUnit { 

  def clearRepository = InMemoryUserRepository.reset
    
  "in-memory user repository" should { clearRepository.before

    val martinOdersky = User("Martin", "Odersky","m.odersky@scala-lang.org","0xcafebabe")

    "assign unique id to user when registering" in { 
      val myriamOdersky = User("Myriam", "Odersky","my.odersky@scala-lang.org","0xcafebabe")
      var u1:User = null
      InMemoryUserRepository.store(martinOdersky){ case Right( user ) => u1 = user }
      var u2:User = null
      InMemoryUserRepository.store(myriamOdersky){ case Right( user ) => u2 = user }

      while( u1 == null && u2 == null ) Thread.sleep(10)
      u1.id must not(be_==(u2.id))
    }


    "lookup user by email" in { 
      val counter = new AtomicInteger(0)

      InMemoryUserRepository.store(martinOdersky){
        user =>

        InMemoryUserRepository.findByStoreKey("m.odersky@scala-lang.org"){
          user => user.get.lastName must be_==("Odersky"); counter.incrementAndGet
        }

        InMemoryUserRepository.findByStoreKey("my.odersky@scala-lang.org"){
          user => user must be_==(None); counter.incrementAndGet
        }

      }

      while( counter.get < 2 ) Thread.sleep(10)

    }
  }
}

