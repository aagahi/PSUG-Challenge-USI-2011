package org.psug.usi.service

import org.psug.usi.store._
import org.psug.usi.store.DataRepositoryMessage
import org.psug.usi.domain.{GameUserHistoryRepository, GameRepository, UserRepository}
import org.psug.usi.netty.Status
import akka.actor.Actor._
import akka.util.Logging
import org.psug.usi.akka.{ActorWrapper, RemoteReceiver}

/**
 * User: alag
 * Date: 2/27/11
 * Time: 2:32 PM
 */


/**
 * Abstract view of services as actors for sending messages.
 */
trait Services {
  val userRepositoryService: ActorWrapper
  val gameRepositoryService: ActorWrapper
  val gameUserHistoryService: ActorWrapper
  val gameManagerService : ActorWrapper
}

/**
 * concrete instances of services viewed remotely as define in akka conf.
 */
class RemoteServices( host:String = "localhost", servicesPort: Int = 2552) extends Services {
  override val userRepositoryService = new RemoteReceiver( "UserRepositoryService", host, servicesPort )
  override val gameRepositoryService = new RemoteReceiver( "GameRepositoryService", host, servicesPort )
  override val gameUserHistoryService = new RemoteReceiver( "GameUserHistoryRepositoryService", host, servicesPort )
  override val gameManagerService = new RemoteReceiver( "GameService", host, servicesPort )
}



/// ---------------------------------------------------------------------------------------
/// ---------------------------------------------------------------------------------------
/// ---------------------------------------------------------------------------------------

trait RepositoryService extends Service with Logging {

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

trait UserRepositoryService extends UserRepository with RepositoryService  {
  override lazy val name = "UserRepositoryService"
}

trait GameRepositoryService extends GameRepository with RepositoryService  {
  override lazy val name = "GameRepositoryService"
}

trait GameUserHistoryRepositoryService extends GameUserHistoryRepository with RepositoryService {
  override lazy val name = "GameUserHistoryRepositoryService"
}

trait RepositoryServices extends Services {
  val userRepositoryService: UserRepositoryService
  val gameRepositoryService: GameRepositoryService
  val gameUserHistoryService: GameUserHistoryRepositoryService
  lazy val gameManagerService : GameManagerService  = new GameManagerService(gameUserHistoryService, userRepositoryService)

  val services : List[Service] = List(userRepositoryService,gameUserHistoryService,gameRepositoryService,gameManagerService)

  def start = {
    remote.start()
    services.foreach(_.go)
  }

  def stop = {

    services.foreach(_.stop )
    remote.shutdown
  }
}

/**
 * Repository services that use a single-instance BDB for storage.
 * Host/port conf is in akka.conf
 */
class SimpleRepositoryServices extends RepositoryServices {
  override lazy val userRepositoryService = new UserRepositoryService {
    override lazy val env = SingleBDBEnvironment
  }
  override lazy val gameRepositoryService = new GameRepositoryService {
    override lazy val env = SingleBDBEnvironment
  }
  override lazy val gameUserHistoryService = new GameUserHistoryRepositoryService {
    override lazy val env = SingleBDBEnvironment
  }
}
