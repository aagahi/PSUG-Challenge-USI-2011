package org.psug.usi.store

/**
 * User: alag
 * Date: 2/16/11
 * Time: 11:26 PM
 */
import scala.actors._
import collection.mutable.HashMap

trait Data[K<:Any] {
  def storeKey:K
  def copyWithAutoGeneratedId( autoGeneratedId:Int ):Data[K]
}


trait DataRepository[K<:Any,T<:Data[K]] extends Actor {

  case class StoreData( data:T )
  case class DataStored( errorEitherData:Either[String,T] )

  case class PullData( key : K )
  case class DataPulled( data:Option[T] )

  case object Clear

  start

  def act {
    loop {
      react {
          case StoreData( data ) => reply( DataStored( store( data ) ) )
          case PullData( key ) => reply( DataPulled( findByStoreKey( key ) ) )
          case Clear => reset
          case x => handleMessage( x )

      }
    }
  }

  protected def handleMessage( any:Any ){
    throw new Exception( "Unexpected message " + any )
  }


    /**
     * @return  true if data contrains are respected
     */
  protected def checkConstraint( data:T ):Boolean

  /**
   * @return  send back a T with same characteristics but with (unique) id defined, or an error message
   */
  protected def store(data : T):Either[String,T]

  /**
   * @return Some(T) if games with id registered in repository, or None
   */
  protected def findByStoreKey(key : K):Option[T]

  /**
   * clear this repository's content.
   */
  protected def reset : Unit
}



/**
 * Stores users in memory, using an actor to serialize access to underlying map.
 */
// TODO: should not use K<:Any for the storeKey...
class InMemoryDataRepository[K<:Any,T<:Data[K]] extends DataRepository[K,T] {

  private var currentId = 0
  protected var dataByKey  : HashMap[K,T] = new HashMap()

  override protected def checkConstraint(data:T) = true
  
  override protected def store(data : T) = {
    currentId += 1
    val copyData = data.copyWithAutoGeneratedId(currentId).asInstanceOf[T]
    if( checkConstraint( copyData ) ){
      dataByKey( copyData.storeKey ) = copyData
      Right( copyData )
    }
    else {
      Left( "Invalid store contraint" )
    }

  }

  override protected def findByStoreKey(key : K) = dataByKey.get(key)


  override protected def reset { currentId = 0;  dataByKey.clear }
}
