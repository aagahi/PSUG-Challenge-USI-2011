package org.psug.usi.users

import org.specs.runner.JUnit4
import org.specs._

class UsersSpec extends SpecificationWithJUnit { 

  "in-memory user repository" should { 

    "assign unique id to user when registering" in { 
      val martinOdersky = User(0,"Martin", "Odersky","m.odersky@scala-lang.org","0xcafebabe")
      val myriamOdersky = User(0,"Myriam", "Odersky","my.odersky@scala-lang.org","0xcafebabe")
      val u1 = inMemoryUserRepository.store(martinOdersky).right.get
      val u2 = inMemoryUserRepository.store(myriamOdersky).right.get
      u1.id must not(be_==(u2.id))
    }

    "lookup user by email" in { 
      val martinOdersky = User(0,"Martin", "Odersky","m.odersky@scala-lang.org","0xcafebabe")
      inMemoryUserRepository.store(martinOdersky)
      inMemoryUserRepository.findByEmail("m.odersky@scala-lang.org").get.lastName must be_==("Odersky")
    }
  }
}

