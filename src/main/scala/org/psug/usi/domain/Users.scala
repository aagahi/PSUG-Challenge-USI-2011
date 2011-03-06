package org.psug.usi.domain

import org.psug.usi.store._
import com.sleepycat.bind.tuple.{IntegerBinding, StringBinding}
import com.sleepycat.je.{LockMode, DatabaseEntry}

object User{
    def apply(firstName : String, lastName : String, email : String, password : String):User = User( 0, firstName, lastName, email, password )
}

case class Credentials(email: String, password: String)

case class User( id:Int, firstName:String, lastName:String, email:String, password:String ) extends Data[Int]{
  def storeKey:Int = id
  def copyWithAutoGeneratedId( id:Int ) = User( id, firstName, lastName, email, password )

  /*
    les longueurs min/max sont les suivantes : de 2 à 50 caractères pour nom, prénom, mail et password.
   */
  def isValid = {
    firstName.length >= 2 && firstName.length <= 50 &&
    lastName.length >= 2 && lastName.length <= 50 &&
    email.length >= 2 && email.length <= 50 &&
    password.length >= 2 && password.length <= 50
  }



}

case class PullDataByEmail( email : String ) extends DataRepositoryMessage

class UserRepository extends BDBDataRepository[User]( "UserRepository" ) {

  override def store( id:Int, in:User ){
    val key = new DatabaseEntry()
    StringBinding.stringToEntry( in.email, key )

    val data = new DatabaseEntry()
    IntegerBinding.intToEntry( id, data )
    database.put( null, key, data )

    super.store( id, in )
  }

  def idByEmail( email:String ) = {
    val key = new DatabaseEntry()
    StringBinding.stringToEntry( email, key )

    val data = new DatabaseEntry()
    database.get( null, key, data, LockMode.READ_UNCOMMITTED )
    if( data.getData != null ){
      Some( IntegerBinding.entryToInt(data ) )
    }
    else None
  }


  /*
    si un utilisateur ayant la même adresse mail existe déjà, une erreur est retournée.
   */
  override protected def checkConstraint( user:User )={
    user.isValid && idByEmail( user.email ).isEmpty
  }

  override def handleMessage( any:Any )={
    any match {
      case PullDataByEmail( email ) =>
        val user = idByEmail( email ).flatMap( id => load( id ) )
        DataPulled[Int]( user )
      case _ => super.handleMessage( any )
    }
  }
}
