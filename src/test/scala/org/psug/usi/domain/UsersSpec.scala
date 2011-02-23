package org.psug.usi.domain

import org.specs._

class UsersSpec extends SpecificationWithJUnit { 
  import UserRepository._
  def clearRepository = UserRepository ! UserRepository.Clear
    
  "in-memory user repository" should { clearRepository.before

    val martinOdersky = User("Martin", "Odersky","m.odersky@scala-lang.org","0xcafebabe")

    "assign unique id to user when registering" in { 
      val myriamOdersky = User("Myriam", "Odersky","my.odersky@scala-lang.org","0xcafebabe")

      val DataStored( Right( u1 ) ) = UserRepository !? StoreData(martinOdersky)
      val DataStored( Right( u2 ) ) = UserRepository !? StoreData(myriamOdersky)

      u1.id must not(be_==(u2.id))
    }


    "lookup user by email" in { 
      
      UserRepository !? UserRepository.StoreData(martinOdersky)

      val DataPulled( Some( user ) ) = UserRepository !? UserRepository.PullDataByEmail("m.odersky@scala-lang.org")
      user.lastName must be_==("Odersky")

      val DataPulled( nouser ) = UserRepository !? UserRepository.PullDataByEmail("my.odersky@scala-lang.org")
      nouser must be_==( None )


    }
  }
}

