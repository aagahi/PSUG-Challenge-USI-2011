package org.psug.usi.domain

import org.specs._
import org.psug.usi.service.UserRepositoryService
import org.psug.usi.domain.UserRepository._

class UsersSpec extends SpecificationWithJUnit { 

  def clearRepository = UserRepositoryService.remoteRef ! ClearRepository()
    
  "in-memory user repository" should { clearRepository.before

    val martinOdersky = User("Martin", "Odersky","m.odersky@scala-lang.org","0xcafebabe")

    "assign unique id to user when registering" in { 
      val myriamOdersky = User("Myriam", "Odersky","my.odersky@scala-lang.org","0xcafebabe")

      val DataStored( Right( u1 ) ) = UserRepositoryService.remoteRef !? StoreData(martinOdersky)
      val DataStored( Right( u2 ) ) = UserRepositoryService.remoteRef !? StoreData(myriamOdersky)

      u1.id must not(be_==(u2.id))
    }


    "lookup user by email" in { 
      
      UserRepositoryService.remoteRef !? StoreData(martinOdersky)

      val DataPulled( Some( user ) ) = UserRepositoryService.remoteRef !? PullDataByEmail("m.odersky@scala-lang.org")
      user.lastName must be_==("Odersky")

      val DataPulled( nouser ) = UserRepositoryService.remoteRef !? PullDataByEmail("my.odersky@scala-lang.org")
      nouser must be_==( None )


    }
  }
}

