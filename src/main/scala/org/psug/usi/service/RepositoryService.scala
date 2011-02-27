package org.psug.usi.service

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
            handleMessage( x ) match {
              case message:DataRepositoryMessage => reply( message )
              case _ =>
            }
      }
    }
  }

  def handleMessage( any:Any ):Any
}


object UserRepositoryService extends RepositoryService{
  override def handleMessage( any:Any ) = UserRepository.handleMessage( any )
}
object GameRepositoryService extends RepositoryService{
  override def handleMessage( any:Any ) = GameRepository.handleMessage( any )
}
