package org.psug.usi.store

/**
 * User: alag
 * Date: 2/16/11
 * Time: 11:26 PM
 */
import scala.actors._
import scala.actors.Actor._
import collection.mutable.HashMap

trait Data[K<:Any] {
  def storeKey:K
  def copyWithAutoGeneratedId( autoGeneratedId:Int ):Data[K]
}

trait DataRepository[K<:Any,T<:Data[K]] {


  /**
   * @return a T with same characteristics but with (unique) id defined, or an error message
   */
  def store(data : T)( callback:(Either[String,T])=>Unit )

  /**
   * @return Some(T) if games with id registered in repository, or None
   */
  def findByStoreKey(key : K)( callback:(Option[T])=>Unit )

  /**
   * clear this repository's content.
   */
  def reset : Unit
}



/**
 * Stores users in memory, using an actor to serialize access to underlying map.
 */
// TODO: should not use K<:Any for the storeKey...
class InMemoryDataRepository[K<:Any,T<:Data[K]] extends DataRepository[K,T] {

  case object Clear

  val store = new Actor {
    start
    private var currentId = 1
    private var dataByKey  : HashMap[K,T] = new HashMap()

    def act {
      loop {
        react {
            case data:T =>
              currentId += 1
              val copyData = data.copyWithAutoGeneratedId(currentId).asInstanceOf[T]
              dataByKey( copyData.storeKey ) = copyData
              reply( copyData )

            case Clear =>
              dataByKey.clear

            case id : K =>
              reply(dataByKey.get(id))

        }
      }
    }
  }

  override def store(data : T)( callback:(Either[String,T])=>Unit ){
    actor{
      store ! data
      react { case data:T=> callback( Right( data ) ) }
    }
  }

  override def findByStoreKey(key : K)( callback:(Option[T])=>Unit ){
      actor{
        store ! key
        react { case data:Option[T]=> callback( data ) }
      }
  }

  override def reset : Unit = store ! Clear
}
