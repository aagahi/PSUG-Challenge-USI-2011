package org.psug.usi.users

import java.util.concurrent.atomic.AtomicInteger
import scala.actors._
import collection.mutable.HashMap

object User{
    def apply(firstName : String, lastName : String, email : String, password : String):User = User( 0, firstName, lastName, email, password )
}
case class User( id : Int, firstName : String, lastName : String, email : String, password : String)

trait UserRepository { 
  

  /**
   * @return a User with same characteristics but with (unique) id defined, or an error message
   */
  def store(user : User) : Either[String,User]

  /**
   * @return Some(user) if user with email registered in repository, or None
   */
  def findByEmail(email : String) : Option[User]

  /**
   * clear this repository's content.
   */
  def reset : Unit
}


/**
 * Stores users in memory, using an actor to serialize access to underlying map.
 */
object InMemoryUserRepository extends UserRepository {

  case object Clear 

  val usersStore = new Actor { 
    private var currentId = 1
    private var usersByEmail  : HashMap[String,User] = new HashMap()
    
    def act {
      loop {
	  react {
        case User(_,firstname,lastname,email,password) =>
          currentId += 1
	      val user = User(currentId,firstname,lastname,email,password)
	      usersByEmail(email) = user
	      reply(user)

	  case email : String => 
	    reply(usersByEmail.get(email))

	  case Clear =>
	    usersByEmail.clear
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

  override def reset : Unit = usersStore ! Clear
}
