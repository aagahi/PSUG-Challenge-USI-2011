package org.psug.usi.domain

import org.specs._

class UsersSpec extends SpecificationWithJUnit { 
  import InMemoryUserRepository._
  def clearRepository = InMemoryUserRepository ! InMemoryUserRepository.Clear
    
  "in-memory user repository" should { clearRepository.before

    val martinOdersky = User("Martin", "Odersky","m.odersky@scala-lang.org","0xcafebabe")

    "assign unique id to user when registering" in { 
      val myriamOdersky = User("Myriam", "Odersky","my.odersky@scala-lang.org","0xcafebabe")

      val DataStored( Right( u1 ) ) = InMemoryUserRepository !? StoreData(martinOdersky)
      val DataStored( Right( u2 ) ) = InMemoryUserRepository !? StoreData(myriamOdersky)

      u1.id must not(be_==(u2.id))
    }


    "lookup user by email" in { 
      
      InMemoryUserRepository !? InMemoryUserRepository.StoreData(martinOdersky)

      val DataPulled( Some( user ) ) = InMemoryUserRepository !? InMemoryUserRepository.PullDataByEmail("m.odersky@scala-lang.org")
      user.lastName must be_==("Odersky")

      val DataPulled( nouser ) = InMemoryUserRepository !? InMemoryUserRepository.PullDataByEmail("my.odersky@scala-lang.org")
      nouser must be_==( None )


    }
  }
}

