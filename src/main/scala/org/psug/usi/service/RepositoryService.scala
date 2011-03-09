package org.psug.usi.service

import org.psug.usi.store._
import org.psug.usi.store.DataRepositoryMessage
import org.psug.usi.domain.{GameRepository, UserRepository}

/**
 * User: alag
 * Date: 2/27/11
 * Time: 2:32 PM
 */

trait RepositoryService extends RemoteService {

  def act {
    loop {
      react {
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

trait RepositoryServices {
  val userRepositoryService: UserRepository with RepositoryService
  val gameRepositoryService: GameRepository with RepositoryService
}

object SimpleRepositoryServices extends RepositoryServices {
  override val userRepositoryService = new UserRepository with RepositoryService {
            override lazy val env = SingleBDBEnvironment
    }
  override val gameRepositoryService = new GameRepository with RepositoryService {
            override lazy val env = SingleBDBEnvironment
  }
}

object DefaultRepositoryServices extends RepositoryServices {
  override val userRepositoryService = UserRepositoryService
  override val gameRepositoryService = GameRepositoryService
}

object UserRepositoryService extends UserRepository with RepositoryService {
  override lazy val env = ReplicatedBDBEnvironment
}

object GameRepositoryService extends GameRepository with RepositoryService {
  override lazy val env = ReplicatedBDBEnvironment
}
