package org.psug.usi.domain

import org.specs._
import org.psug.usi.service.{ClientServices, ServerServices}
import org.psug.usi.store._

class UsersSpec extends SpecificationWithJUnit {

  val services = new ClientServices()
  import services._

  val serverServices = new ServerServices

  def startRepository : Unit = {
    serverServices.launch
    userRepositoryService !? ClearRepository
  }

  def clearRepository =  {
    userRepositoryService !? ClearRepository
    serverServices.shutdown
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

      val DataPulled( Some( user1 ) ) = userRepositoryService !? PullData( u1.asInstanceOf[User].id )
      user1 must be_==( u1 )

      val DataPulled( Some( user2 ) ) = userRepositoryService !? PullData( u2.asInstanceOf[User].id )
      user2 must be_==( u2 )

      val DataPulled( Some( userLast ) ) = userRepositoryService !? PullLast
      userLast must be_==( u2 )
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

