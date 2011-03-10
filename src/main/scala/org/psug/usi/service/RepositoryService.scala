package org.psug.usi.service

import org.psug.usi.store._
import org.psug.usi.store.DataRepositoryMessage
import org.psug.usi.domain.{GameRepository, UserRepository}
import org.psug.usi.system.{NodeTypes, Status}

/**
 * User: alag
 * Date: 2/27/11
 * Time: 2:32 PM
 */

trait RepositoryService extends DefaultServiceConfiguration with Service {

  def act {
    loop {
      react {
        case ServiceStatus => reply(Status(NodeTypes.service, port))
        case Exit => println("service " + symbol + " exiting"); exit()
        case x =>
          handleMessage(x) match {
            case message: DataRepositoryMessage => reply(message)
            case _ =>
          }
      }
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
}

/**
 * concrete instances of services viewed remotely.
 */
class RemoteServices(servicesPort : Int = 55555) extends Services {

  override val userRepositoryService = new RemoteService with DefaultServiceConfiguration {
    override lazy val port = servicesPort
    override lazy val symbol = 'UserRepositoryService
  }
  override val gameRepositoryService = new RemoteService with DefaultServiceConfiguration {
    override lazy val port = servicesPort
    override lazy val symbol = 'GameRepositoryService
  }
}

trait UserRepositoryService extends UserRepository with RepositoryService with RemoteService {
  override lazy val symbol = 'UserRepositoryService
}

trait GameRepositoryService extends GameRepository with RepositoryService with RemoteService {
  override lazy val symbol = 'GameRepositoryService
}

trait RepositoryServices extends Services {
  val userRepositoryService: UserRepositoryService
  val gameRepositoryService: GameRepositoryService

  def start = {
    gameRepositoryService.go
    userRepositoryService.go
  }

  def stop = {
    gameRepositoryService ! Exit
    userRepositoryService ! Exit
  }
}

/**
 * Repository services that use a single-instance BDB for storage.
 */
class SimpleRepositoryServices(servicesPort: Int = 55555) extends RepositoryServices {
  override val userRepositoryService = new UserRepositoryService {
    override lazy val port = servicesPort
    override lazy val env = SingleBDBEnvironment
  }
  override val gameRepositoryService = new GameRepositoryService {
    override lazy val port = servicesPort
    override lazy val env = SingleBDBEnvironment
  }

}

/**
 * Repository services that use a replicated BDB environment for storage.
 */
class DefaultRepositoryServices extends RepositoryServices {
  override val userRepositoryService = new UserRepositoryService {
    override lazy val env = ReplicatedBDBEnvironment
  }
  override val gameRepositoryService = new GameRepositoryService {
    override lazy val env = ReplicatedBDBEnvironment
  }
}


