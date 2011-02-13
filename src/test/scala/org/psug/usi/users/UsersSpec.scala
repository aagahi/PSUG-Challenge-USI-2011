package org.psug.usi.users

import org.specs.runner.JUnit4
import org.specs._

class UsersSpec extends SpecificationWithJUnit { 

  def clearRepository = InMemoryUserRepository.reset
    
  "in-memory user repository" should { clearRepository.before

    val martinOdersky = User(0,"Martin", "Odersky","m.odersky@scala-lang.org","0xcafebabe")

    "assign unique id to user when registering" in { 
      val myriamOdersky = User(0,"Myriam", "Odersky","my.odersky@scala-lang.org","0xcafebabe")
      val u1 = InMemoryUserRepository.store(martinOdersky).right.get
      val u2 = InMemoryUserRepository.store(myriamOdersky).right.get
      u1.id must not(be_==(u2.id))
    }

    "lookup user by email" in { 
      InMemoryUserRepository.store(martinOdersky)
      InMemoryUserRepository.findByEmail("m.odersky@scala-lang.org").get.lastName must be_==("Odersky")
      InMemoryUserRepository.findByEmail("my.odersky@scala-lang.org") must be_==(None)
    }
  }
}

