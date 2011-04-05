package org.psug.usi.service

import org.psug.usi.store._
import org.psug.usi.store.DataRepositoryMessage
import org.psug.usi.domain.{GameUserHistoryRepository, GameRepository, UserRepository}
import org.psug.usi.netty.Status
import akka.actor.Actor._
import akka.util.Logging
import org.psug.usi.akka.{Receiver, ActorWrapper, RemoteReceiver}

/**
 * User: alag
 * Date: 2/27/11
 * Time: 2:32 PM
 */

trait UserRepositoryService extends ActorWrapper
trait GameRepositoryService extends ActorWrapper
trait GameUserHistoryRepositoryService extends ActorWrapper
trait GameManagerService extends ActorWrapper

/**
 * Abstract view of services as actors for sending messages.
 */
trait Services {
  val userRepositoryService: UserRepositoryService
  val gameRepositoryService: GameRepositoryService
  val gameUserHistoryService: GameUserHistoryRepositoryService
  val gameManagerService : GameManagerService
}

/**
 * client instances of services viewed remotely as define in akka conf.
 */
class ClientServices( host:String = "localhost", servicesPort: Int = 2552) extends Services {
  override val userRepositoryService = new RemoteReceiver( "UserRepositoryService", host, servicesPort ) with UserRepositoryService
  override val gameRepositoryService = new RemoteReceiver( "GameRepositoryService", host, servicesPort ) with GameRepositoryService
  override val gameUserHistoryService = new RemoteReceiver( "GameUserHistoryRepositoryService", host, servicesPort ) with GameUserHistoryRepositoryService
  override val gameManagerService = new RemoteReceiver( "GameManager", host, servicesPort ) with GameManagerService
}



/// ---------------------------------------------------------------------------------------
/// ---------------------------------------------------------------------------------------
/// ---------------------------------------------------------------------------------------

trait RepositoryService extends Receiver with Service with Logging {

  def receive = {
    case ServiceStatus => reply(Status( "Service" ))
    case StopReceiver => log.info("service " + name + " exiting"); stop()
    case x =>
      try {
        handleMessage(x) match {
          case message: DataRepositoryMessage => reply(message)
          case x => log.warn( "Unknown message: " + x )
        }
      } catch {
        case e => log.error( e, e.getMessage )
      }
  }

  def handleMessage(any: Any): Any
}


trait ServerServices extends Services {
  
  val services : List[Service]

  def launch = {
    remote.start()
    services.foreach(_.launch)
  }

  def shutdown = {

    services.foreach(_.shutdown )
    remote.shutdown
  }
}

/**
 * Repository services that use a single-instance BDB for storage.
 * Host/port conf is in akka.conf
 */
class SimpleRepositoryServices extends ServerServices {
  override val services = List(userRepositoryService,gameUserHistoryService,gameRepositoryService,gameManagerService)

  override lazy val userRepositoryService = new UserRepository with RepositoryService with UserRepositoryService {
    override lazy val name = "UserRepositoryService"
    override lazy val env = SingleBDBEnvironment
  }
  override lazy val gameRepositoryService = new GameRepository with RepositoryService with GameRepositoryService {
    override lazy val name = "GameRepositoryService"
    override lazy val env = SingleBDBEnvironment
  }
  override lazy val gameUserHistoryService = new GameUserHistoryRepository with RepositoryService with GameUserHistoryRepositoryService {
    override lazy val name = "GameUserHistoryRepositoryService"
    override lazy val env = SingleBDBEnvironment
  }

  override lazy val gameManagerService = new GameManager(gameUserHistoryService, userRepositoryService) with Service with GameManagerService {
    override lazy val name = "GameManager"
  }

}
