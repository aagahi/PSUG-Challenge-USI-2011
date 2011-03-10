package org.psug.usi.domain

import org.specs._
import org.psug.usi.service.SimpleRepositoryServices
import org.psug.usi.store.{ClearRepository, DataPulled, DataStored, StoreData}

class UsersSpec extends SpecificationWithJUnit {

  val repositories = new SimpleRepositoryServices
  import repositories._

  def startRepository = {
    start
  }

  def clearRepository =  {
    userRepositoryService.remote !? ClearRepository
    exit
  }

  "in-memory user repository" should {

    startRepository.before
    clearRepository.after

    val martinOdersky = User("Martin", "Odersky","m.odersky@scala-lang.org","0xcafebabe")

    "assign unique id to user when registering" in { 
      val myriamOdersky = User("Myriam", "Odersky","my.odersky@scala-lang.org","0xcafebabe")

      val DataStored( Right( u1 ) ) = userRepositoryService.remote !? StoreData(martinOdersky)
      val DataStored( Right( u2 ) ) = userRepositoryService.remote !? StoreData(myriamOdersky)

      u1.asInstanceOf[User].id must not(be_==(u2.asInstanceOf[User].id))
    }


    "lookup user by email" in { 
      
      userRepositoryService.remote !? StoreData(martinOdersky)

      val DataPulled( Some( user ) ) = userRepositoryService.remote !? PullDataByEmail("m.odersky@scala-lang.org")
      user.asInstanceOf[User].lastName must be_==("Odersky")

      val DataPulled( nouser ) = userRepositoryService.remote !? PullDataByEmail("my.odersky@scala-lang.org")
      nouser must be_==( None )

    }
  }
}

