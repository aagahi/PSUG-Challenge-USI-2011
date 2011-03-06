package org.psug.usi.rest

/**
 * User: alag
 * Date: 2/22/11
 * Time: 1:28 AM
 */


import org.specs._

import com.sun.jersey.api.client._

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import org.psug.usi.netty._
import org.psug.usi.domain.User
import org.psug.usi.domain.Credentials
import org.psug.usi.service.UserRepositoryService
import org.psug.usi.store.ClearRepository

class NettyUserRegistrationSpec  extends SpecificationWithJUnit {

  implicit val formats = Serialization.formats(NoTypeHints)

  val martinOdersky = User( "Martin", "Odersky","m.odersky@scala-lang.org","0xcafebabe")
  val myriamOdersky = User("Myriam", "Odersky","m.odersky@scala-lang.org","0xbabecafe")

  val listenPort = 12345

  def webResource( path:String ) = new Client().resource("http://localhost:"+listenPort+path)

  new SpecContext {
    val webServer : WebServer = new WebServer(listenPort)

    // start/stop web server on each Specification
    beforeSpec(webServer.start)
    afterSpec(webServer.stop)

    // clear repository on each example
    before(UserRepositoryService !!  ClearRepository)
  }

  def registerUser(user : User) : String = {
     webResource("/api/user/").header("Content-Type","application/json").post(classOf[String], Serialization.write(martinOdersky))
  }

  def userLogsIn(credentials: Credentials) : String = {
     webResource("/api/login/").header("Content-Type","application/json").post(classOf[String], Serialization.write(credentials))
  }

  "user registration" should {
    shareVariables()
    
    "succeeds if user does not exist" in { 
      
      read[User](registerUser(martinOdersky)).id must be_==(1)

      val expectedUser = martinOdersky.copyWithAutoGeneratedId( 1 )
      val user = read[User]( webResource("/api/user/1").get(classOf[String]) )
      user must be_==(expectedUser)
    }
    
    "fail if user with same email exists" in {     
      registerUser(martinOdersky)
      registerUser(myriamOdersky) must throwA[UniformInterfaceException]
    }
  }

  "user login" should {
    shareVariables()

    "succeed with returned session cookie if user provide right credentials" in {
      registerUser(martinOdersky)
      userLogsIn(Credentials("m.odersky@scala-lang.org", "0xcafebabe"))
    }

  }

}
