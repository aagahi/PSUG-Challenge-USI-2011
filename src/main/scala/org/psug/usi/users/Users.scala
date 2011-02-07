package org.psug.usi.users

import java.util.concurrent.atomic.AtomicInteger
import scala.actors._

case class User(id : Int, firstName : String, lastName : String, mail : String, password : String)

trait UserRepository { 

  /**
   * @return a User with same characteristics but with (unique) id defined, or an error message
   */
  def store(user : User) : Either[String,User]

  /**
   * @return Some(user) if user with email registered in repository, or None
   */
  def findByEmail(email : String) : Option[User]

}


/**
 * Stores users in memory, using an actor to serialize access to underlying map.
 */
object inMemoryUserRepository extends UserRepository { 

  val usersStore = new Actor { 
    val nextId : AtomicInteger    = new AtomicInteger
    var users  : Map[String,User] = Map() 
    
    def act {
      loop {
	react {
          case User(_,f,l,m,p) => {
	    val id = nextId.incrementAndGet
	    val idUser = User(id,f,l,m,p)
	    users = users + (m -> idUser)
	    reply(idUser)
          }
	  case email : String => 
	    reply(users.get(email))
	}
      }
    }
  }.start

  override def store(user : User) : Either[String,User]  = { 
    Right((usersStore !? user).asInstanceOf[User])
  }
 
  override def findByEmail(email : String) : Option[User] = { 
    (usersStore !? email).asInstanceOf[Option[User]]
  }
}
