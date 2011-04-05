package org.psug.usi.domain

import org.specs._
import org.psug.usi.service.SimpleRepositoryServices
import org.psug.usi.store.{ClearRepository, DataPulled, DataStored, StoreData}

class UsersSpec extends SpecificationWithJUnit {

  val repositories = new SimpleRepositoryServices
  import repositories._

  def startRepository : Unit = {
    start
    userRepositoryService !? ClearRepository
  }

  def clearRepository =  {
    userRepositoryService !? ClearRepository
    stop
  }

  "in-memory user repository" should {

    startRepository.before
    clearRepository.after

    val martinOdersky = User("Martin", "Odersky","m.odersky@scala-lang.org","0xcafebabe")

    "assign unique id to user when registering" in { 
      val myriamOdersky = User("Myriam", "Odersky","my.odersky@scala-lang.org","0xcafebabe")

      val DataStored( Right( u1 ) ) = userRepositoryService !? StoreData(martinOdersky)
      val DataStored( Right( u2 ) ) = userRepositoryService !? StoreData(myriamOdersky)

      u1.asInstanceOf[User].id must not(be_==(u2.asInstanceOf[User].id))
    }


    "lookup user by mail" in {

      var novalue = userRepositoryService !? PullDataByEmail("m.odersky@scala-lang.org")
      novalue must be_==( DataPulled( None ) )


      userRepositoryService !? StoreData(martinOdersky)

      val DataPulled( Some( user ) ) = userRepositoryService !? PullDataByEmail("m.odersky@scala-lang.org")
      user.asInstanceOf[User].lastname must be_==("Odersky")

      val DataPulled( nouser ) = userRepositoryService !? PullDataByEmail("my.odersky@scala-lang.org")
      nouser must be_==( None )

    }
  }
}

