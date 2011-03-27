package org.psug.usi.service

import org.psug.usi.store._
import org.psug.usi.store.DataRepositoryMessage
import org.psug.usi.domain.{GameUserHistoryRepository, GameRepository, UserRepository}
import org.psug.usi.netty.Status
import akka.actor.Actor._
import akka.util.Logging


/**
 * User: alag
 * Date: 2/27/11
 * Time: 2:32 PM
 */

trait RepositoryService extends DefaultServiceConfiguration with Service with Logging {

  def receive = {
    case ServiceStatus => reply(Status( "Service", port))
    case StopReceiver => log.info("service " + name + " exiting"); stop()
    case x =>
      handleMessage(x) match {
        case message: DataRepositoryMessage => reply(message)
        case _ =>
      }
  }

  def handleMessage(any: Any): Any
}

/**
 * Abstract view of services as actors for sending messages.
 */
trait Services {
  val userRepositoryService: RemoteService
  val gameRepositoryService: RemoteService
  val gameUserHistoryService: RemoteService
  val gameManagerService : RemoteService
}

/**
 * concrete instances of services viewed remotely.
 */
class RemoteServices(servicesPort: Int = 2552) extends Services {

  override val userRepositoryService = new RemoteService with DefaultServiceConfiguration {
    override lazy val port = servicesPort
    override lazy val name = "UserRepositoryService"
  }
  override val gameRepositoryService = new RemoteService with DefaultServiceConfiguration {
    override lazy val port = servicesPort
    override lazy val name = "GameRepositoryService"
  }
  override val gameUserHistoryService = new RemoteService with DefaultServiceConfiguration {
    override lazy val port = servicesPort
    override lazy val name = "GameUserHistoryRepositoryService"
  }
  override val gameManagerService = new RemoteService with DefaultServiceConfiguration {
    override lazy val port = servicesPort
    override lazy val name = "GameService"
  }
}

trait UserRepositoryService extends UserRepository with RepositoryService with RemoteService {
  override lazy val name = "UserRepositoryService"
}

trait GameRepositoryService extends GameRepository with RepositoryService with RemoteService {
  override lazy val name = "GameRepositoryService"
}

trait GameUserHistoryRepositoryService extends GameUserHistoryRepository with RepositoryService with RemoteService {
  override lazy val name = "GameUserHistoryRepositoryService"
}

trait RepositoryServices extends Services {
  val userRepositoryService: UserRepositoryService
  val gameRepositoryService: GameRepositoryService
  val gameUserHistoryService: GameUserHistoryRepositoryService
  lazy val gameManagerService : GameManagerService  = new GameManagerService(gameUserHistoryService)

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
 */
class SimpleRepositoryServices(servicesPort: Int = 2552) extends RepositoryServices {
  override lazy val userRepositoryService = new UserRepositoryService {
    override lazy val port = servicesPort
    override lazy val env = SingleBDBEnvironment
  }
  override lazy val gameRepositoryService = new GameRepositoryService {
    override lazy val port = servicesPort
    override lazy val env = SingleBDBEnvironment
  }
  override lazy val gameUserHistoryService = new GameUserHistoryRepositoryService {
    override lazy val port = servicesPort
    override lazy val env = SingleBDBEnvironment
  }
}

/**
 * Repository services that use a replicated BDB environment for storage.
 */
class DefaultRepositoryServices extends RepositoryServices {
  override lazy val userRepositoryService = new UserRepositoryService {
    override lazy val env = ReplicatedBDBEnvironment
  }
  override lazy val gameRepositoryService = new GameRepositoryService {
    override lazy val env = ReplicatedBDBEnvironment
  }
  override lazy val gameUserHistoryService = new GameUserHistoryRepositoryService {
    override lazy val env = ReplicatedBDBEnvironment
  }
}


