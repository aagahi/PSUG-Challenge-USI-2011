package org.psug.usi.service

import org.psug.usi.domain.UserRepository
import org.psug.usi.store.DataRepositoryMessage

/**
 * User: alag
 * Date: 2/26/11
 * Time: 5:24 PM
 */


// TODO: Refactor with GameRepository
object UserRepositoryService extends CommonService {

  override val symbol = Symbol("UserRepositoryService")

  registerAsRemoteActor

  def act {
    loop {
      react {
          case x =>
            UserRepository.handleMessage( x ) match {
              case message:DataRepositoryMessage => reply( message )
              case _ =>
            }
      }
    }
  }
  
}
