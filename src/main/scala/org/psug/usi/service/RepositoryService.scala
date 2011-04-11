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
  def userRepositoryService: UserRepositoryService
  def gameRepositoryService: GameRepositoryService
  def gameUserHistoryService: GameUserHistoryRepositoryService
  def gameManagerService : GameManagerService
}

/**
 * client instances of services viewed remotely as define in akka conf.
 */
class ClientServices( host:String = "localhost", servicesPort: Int = 2552) extends Services {
  override def userRepositoryService = new RemoteReceiver( "UserRepositoryService", host, servicesPort ) with UserRepositoryService
  override def gameRepositoryService = new RemoteReceiver( "GameRepositoryService", host, servicesPort ) with GameRepositoryService
  override def gameUserHistoryService = new RemoteReceiver( "GameUserHistoryRepositoryService", host, servicesPort ) with GameUserHistoryRepositoryService
  override def gameManagerService = new RemoteReceiver( "GameManager", host, servicesPort ) with GameManagerService
}



/// ---------------------------------------------------------------------------------------
/// ---------------------------------------------------------------------------------------
/// ---------------------------------------------------------------------------------------

trait RepositoryService extends Receiver with Service with Logging {

  def receive = {
    case ServiceStatus => reply(Status( "Service" ))
    case StopReceiver => log.info("service " + name + " exiting"); stop()
    case x =>
      handleMessage(x) match {
        case message: DataRepositoryMessage => reply(message)
        case x => log.warn( "Unknown message: " + x )
      }
  }

  def handleMessage(any: Any): Any
}




/**
 * Repository services that use a single-instance BDB for storage.
 * Host/port conf is in akka.conf
 */
class ServerServices extends Services {
  private var services:List[Service] = Nil

  private var _userRepositoryService:UserRepositoryService with Service = null
  override def userRepositoryService = _userRepositoryService

  private var _gameRepositoryService:GameRepositoryService with Service = null
  override def gameRepositoryService = _gameRepositoryService

  private var _gameUserHistoryService:GameUserHistoryRepositoryService with Service = null
  override def gameUserHistoryService = _gameUserHistoryService

  private var _gameManagerService:GameManagerService with Service = null
  override def gameManagerService = _gameManagerService



  def launch = {
    remote.start()

    _userRepositoryService = new UserRepository with RepositoryService with UserRepositoryService {
      override lazy val name = "UserRepositoryService"
      override lazy val env = SingleBDBEnvironment
    }

    _gameRepositoryService = new GameRepository with RepositoryService with GameRepositoryService {
      override lazy val name = "GameRepositoryService"
      override lazy val env = SingleBDBEnvironment
    }

    _gameUserHistoryService = new GameUserHistoryRepository with RepositoryService with GameUserHistoryRepositoryService {
      override lazy val name = "GameUserHistoryRepositoryService"
      override lazy val env = SingleBDBEnvironment
    }

    _gameManagerService = new GameManager(this) with Service with GameManagerService {
      override lazy val name = "GameManager"
    }

    services = _userRepositoryService::_gameRepositoryService::_gameUserHistoryService::_gameManagerService::Nil
    services.foreach(_.launch)
  }

  def shutdown = {
    services.foreach(_.shutdown )
    services = Nil
    _userRepositoryService = null
    _gameRepositoryService = null
    _gameManagerService = null
    _gameManagerService = null
    remote.shutdown
  }


}
