package org.psug.usi.store

import com.sleepycat.je._
import com.sleepycat.bind.tuple.{IntegerBinding}
import java.io._
import rep.{NoConsistencyRequiredPolicy, ReplicatedEnvironment, ReplicationConfig}

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
trait BDBEnvironment {
   self : BDBConfiguration =>

  /**
   * Provide a specific Environment instance given a generic configuration.
   */
  def configure(envConfig: EnvironmentConfig): Environment

  var openDatabases = Set[Database]()

  Runtime.getRuntime().addShutdownHook(new ShutdownHook)

  lazy val environment = {
    val envConfig = new EnvironmentConfig()
    envConfig.setAllowCreate(true)

    envConfig.setCacheSize(cacheSizeInByte)
    //envConfig.setCachePercent( 5 )
    envConfig.setSharedCache(true)

    envConfig.setTransactional(true)

    val durability = new Durability(Durability.SyncPolicy.WRITE_NO_SYNC,
      Durability.SyncPolicy.NO_SYNC,
      Durability.ReplicaAckPolicy.NONE)

    envConfig.setDurability(durability)

    println("Starting " + getClass + " environment on " + replicaHostName + " contacting helper " + replicaHelperHostName)

    configure(envConfig)
  }

  def registerDatabase(database: Database) {
    openDatabases += database
  }

  def unregisterDatabase(database: Database) {
    openDatabases -= database
  }

  def shutdown() {
    openDatabases.foreach(_.close)
    environment.sync()
    environment.cleanLog()
    environment.close()
  }

  class ShutdownHook extends Thread {
    override def run() {
      println("Shutting down databases")
      shutdown
    }
  }

}

/**
 * Configuration parameters for BDB environment with default 'sensible' values.
 */
case class BDBConfiguration(
                             cacheSizeInByte: Int = 1024 * 1024 * 8,
                             replicaGroupName: String = "ChallengeUSI",
                             nodeName: String = "Node1",
                             replicaHostName: String = "localhost:5501",
                             replicaHelperHostName: String = "localhost:5501",
                             envHome: File = new File("./target/bdb")
                             )

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

/**
 * A single instance of a database operating in a given environment.
 * This trait needs to be given a proper BDBEnvironment when instantiated.
 */
trait BDB[T <: Data[Int]] {

  val env : BDBEnvironment with BDBConfiguration

  val databaseName: String

  lazy val dbFolder = new File(env.envHome, databaseName)

  private var _database: Database = null

  def database = {
    if (_database == null) {
      if (!dbFolder.exists()) dbFolder.mkdirs()
      val dbConfig = new DatabaseConfig()
      dbConfig.setAllowCreate(true)
      dbConfig.setTransactional(true)
      dbConfig.setDeferredWrite(false)
      _database = env.environment.openDatabase(null, databaseName, dbConfig);
      env.registerDatabase(_database)
    }
    _database
  }

  def close() {
    database.close()
    env.unregisterDatabase(database)
    _database = null
  }

  def removeDatabase() {
    close()
    env.environment.removeDatabase(null, databaseName)
  }

  def store(id: Int, in: T) {
    val tx = env.environment.beginTransaction(null, null)
    val key = new DatabaseEntry()
    IntegerBinding.intToEntry(id, key)

    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(in)

    val data = new DatabaseEntry(baos.toByteArray)
    database.put(null, key, data)

    tx.commit()
  }

  def lastIndex = {
    val cursor = database.openCursor(null, null)
    try {
      val key = new DatabaseEntry()
      val data = new DatabaseEntry()
      cursor.getLast(key, data, LockMode.DEFAULT);
      if (key.getData() != null) {
        IntegerBinding.entryToInt(key)
      }
      else 0
    }
    finally {
      cursor.close()
    }

  }

  def load(id: Int): Option[T] = {
    //val tx = environment.beginTransaction( null, null )
    val key = new DatabaseEntry()
    IntegerBinding.intToEntry(id, key)

    val data = new DatabaseEntry()

    database.get(null, key, data, LockMode.READ_UNCOMMITTED)
    val buffer = data.getData
    if (buffer != null) {
      val ois = new ObjectInputStream(new ByteArrayInputStream(buffer))
      Some(ois.readObject().asInstanceOf[T])


    }
    else None
  }

  def delete(id: Int) {
    //val tx = environment.beginTransaction( null, null )
    val key = new DatabaseEntry()
    IntegerBinding.intToEntry(id, key)
    database.delete(null, key)
    //tx.commit()
  }


}

/**
 * A repository of data backed by a BDB.
 */
abstract class BDBDataRepository[T <: Data[Int]](override val databaseName: String) extends DataRepository[Int, T] with BDB[T] {

  private var currentId = lastIndex

  override protected def checkConstraint(data: T): Option[String] = None

  override protected def store(data: T) = {
    currentId += 1
    val copyData = data.copyWithAutoGeneratedId(currentId).asInstanceOf[T]
    (checkConstraint(copyData)) match {
      case None =>
        store(currentId, copyData)
        Right(copyData)
      case Some(message) =>
        Left("Invalid store contraint: " + message)
    }
  }

  override protected def findByStoreKey(key: Int) = load(key)

  override protected def reset : RepositoryCleared = {
    currentId = 0;
    removeDatabase
    RepositoryCleared(databaseName)
  }
}
