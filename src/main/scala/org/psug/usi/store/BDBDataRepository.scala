package org.psug.usi.store

import com.sleepycat.je._
import com.sleepycat.bind.tuple.IntegerBinding
import java.io._
import rep.{NoConsistencyRequiredPolicy, ReplicatedEnvironment, ReplicationConfig}
import collection.JavaConversions._
import collection.mutable.HashMap
import akka.util.Logging
import java.util.Properties

/**
 * User: alag
 * Date: 3/5/11
 * Time: 1:31 PM
 */

/**
 * A BDB environment.
 * An environment is a configured set of databases accessible through BDB API and residing within a given directory
 * structure on file-system. An environment may be a standalone instance of replicated.
 */
trait BDBEnvironment extends Logging {
   self : BDBConfiguration =>

  val databases = new HashMap[String,BDBManagement]()
  /**
   * Provide a specific Environment instance given a generic configuration.
   */
  def configure(envConfig: EnvironmentConfig): Environment

  log.info( "Adding shutdown hook for databases")
  Runtime.getRuntime().addShutdownHook(new ShutdownHook)

  lazy val environment = {
    val envConfig = new EnvironmentConfig()
    envConfig.setAllowCreate(true)

    envConfig.setCacheSize(cacheSizeInByte)
    //envConfig.setCachePercent( 5 )
    envConfig.setSharedCache(true)

    envConfig.setTransactional(transactional)
    

    val durability = new Durability(Durability.SyncPolicy.WRITE_NO_SYNC,
      Durability.SyncPolicy.NO_SYNC,
      Durability.ReplicaAckPolicy.NONE)

    envConfig.setDurability(durability)

    log.info("Starting " + getClass + " environment on " + replicaHostName + " contacting helper " + replicaHelperHostName)

    configure(envConfig)
  }



  class ShutdownHook extends Thread {
    override def run() {
      log.info("Shutting down databases")
      environment.sync()
      environment.cleanLog()

      environment.getDatabaseNames().foreach{
        dbName =>
        databases.get( dbName ).foreach{ db => log.info( "Closing db: " + db.databaseName ); db.close() }
      }

      environment.close()
    }
  }

}

/**
 * Configuration parameters for BDB environment with default 'sensible' values.
 */
class BDBConfiguration{
  private val properties = new Properties()
  properties.load( getClass.getResourceAsStream( "/db.properties" ) )

  val cacheSizeInByte = properties.getProperty("cacheSizeInByte").toInt
  val replicaGroupName = properties.getProperty("replicaGroupName")
  val nodeName = properties.getProperty("nodeName")
  val replicaHostName = properties.getProperty("replicaHostName")
  val replicaHelperHostName = properties.getProperty("replicaHelperHostName")
  val envHome = new File( properties.getProperty("envHome") )
  val transactional = properties.getProperty("transactional").toBoolean
  val deferredWrite = properties.getProperty("deferredWrite").toBoolean
}

trait SingleInstanceEnvironment extends BDBEnvironment {
  self: BDBConfiguration =>
  override def configure(envConfig: EnvironmentConfig) = new Environment(envHome, envConfig)
}

trait ReplicatedInstancesEnvironment extends BDBEnvironment {
  self: BDBConfiguration =>

  override def configure(envConfig: EnvironmentConfig) = {
    val repConfig = new ReplicationConfig()
    repConfig.setGroupName(replicaGroupName)
    repConfig.setNodeName(nodeName)
    repConfig.setNodeHostPort(replicaHostName)
    repConfig.setDesignatedPrimary(true)
    repConfig.setHelperHosts(replicaHelperHostName)

    repConfig.setConsistencyPolicy(new NoConsistencyRequiredPolicy())

    new ReplicatedEnvironment(envHome, repConfig, envConfig)
  }
}

object ReplicatedBDBEnvironment extends BDBConfiguration with ReplicatedInstancesEnvironment
object SingleBDBEnvironment extends BDBConfiguration with SingleInstanceEnvironment




trait BDBDataFactory[K<:Any,T<:Data[K]] {
  def entryToValue( entry:DatabaseEntry ):Option[T]
  def entryToKey( entry:DatabaseEntry ):Option[K]

  def keyToEntry( key:K ):DatabaseEntry
  def valueToEntry( value:T ):DatabaseEntry
}

class BDBSimpleDataFactory[T<:Data[Int]] extends BDBDataFactory[Int,T]{
  def entryToValue( entry:DatabaseEntry ):Option[T] ={
    val buffer = entry.getData
    if (buffer != null) {
      val ois = new ObjectInputStream(new ByteArrayInputStream(buffer))
      Some(ois.readObject().asInstanceOf[T])
    }
    else None
  }

  def entryToKey( entry:DatabaseEntry ):Option[Int] = {
    if( entry.getData != null ) Some( IntegerBinding.entryToInt( entry ) )
    else None
  }

  def keyToEntry( key:Int ):DatabaseEntry={
    val entry = new DatabaseEntry()
    IntegerBinding.intToEntry( key, entry )
    entry
  }
  def valueToEntry( value:T ):DatabaseEntry={
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(value)
    new DatabaseEntry(baos.toByteArray)
  }
}


/**
 * A single instance of a database operating in a given environment.
 * This trait needs to be given a proper BDBEnvironment when instantiated.
 */
trait BDBManagement extends Logging {
  val env : BDBEnvironment with BDBConfiguration


  val databaseName: String
  lazy val dbFolder = new File(env.envHome, databaseName)

  protected  var _database: Database = null

  def database = {
    if (_database == null) {
      if (!dbFolder.exists()) dbFolder.mkdirs()
      val dbConfig = new DatabaseConfig()
      dbConfig.setAllowCreate(true)
      dbConfig.setTransactional(env.transactional)
      dbConfig.setDeferredWrite(env.deferredWrite)
      _database = env.environment.openDatabase(null, databaseName, dbConfig);
      env.databases( databaseName ) = this
    }
    _database
  }

  def close() {
    if( _database != null ){
      _database.close()
      log.info( databaseName + " closed")
    }

    _database = null
  }


  def removeDatabase() {
    log.debug( "Remove db: " + databaseName )
    close()
    try{
      env.environment.truncateDatabase(null, databaseName, false )
    }
    catch {
      case _ => log.warn( "Error on removing database " + databaseName )
    }
  }

}
trait BDB[K<:Any,T<:Data[K]] extends BDBManagement {

  val dataFactory:BDBDataFactory[K,T]




  def save(in:T) {
    database.put(null, dataFactory.keyToEntry(in.storeKey), dataFactory.valueToEntry(in) )
  }

  def last = {
    val cursor = database.openCursor(null, null)
    try {
      val key = new DatabaseEntry()
      val data = new DatabaseEntry()
      cursor.getLast(key, data, LockMode.DEFAULT);
      dataFactory.entryToKey( key )
    }
    finally {
      cursor.close()
    }

  }

  def load(key:K) = {
    //val tx = environment.beginTransaction( null, null )
    val keyEntry = dataFactory.keyToEntry( key )
    val valueEntry = new DatabaseEntry()
    database.get(null, keyEntry, valueEntry, LockMode.READ_UNCOMMITTED)
    dataFactory.entryToValue( valueEntry )
  }

  def delete(key:K) {
    //val tx = environment.beginTransaction( null, null )
    val keyEntry = dataFactory.keyToEntry( key )
    database.delete(null, keyEntry)
    //tx.commit()
  }


}

/**
 * A repository of data backed by a BDB.
 */
abstract class BDBDataRepository[K<:Any,T<:Data[K]](override val databaseName: String, override val dataFactory:BDBDataFactory[K,T])
  extends DataRepository[K,T] with BDB[K,T] {

  var currentId = last.getOrElse(currentIdResetValue)

  def incrementAndGetCurrentId:K
  def currentIdResetValue:K


  override protected def checkConstraint(data: T): Option[String] = None

  override protected def store(data: T) = {
    val copyData = data.copyWithAutoGeneratedId(incrementAndGetCurrentId).asInstanceOf[T]
    (checkConstraint(copyData)) match {
      case None =>
        save(copyData)
        Right(copyData)
      case Some(message) =>
        Left("Invalid store contraint: " + message)
    }
  }

  override def findByStoreKey(key:K) = load(key)

  override def findLast() = load(currentId)


  override protected def reset : RepositoryCleared = {
    currentId = currentIdResetValue
    removeDatabase
    RepositoryCleared(databaseName)
  }
}
