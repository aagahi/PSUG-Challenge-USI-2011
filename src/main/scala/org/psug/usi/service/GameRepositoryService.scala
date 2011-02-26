package org.psug.usi.service

import org.psug.usi.domain.GameRepository
import org.psug.usi.store.DataRepositoryMessage

/**
 * User: alag
 * Date: 2/26/11
 * Time: 5:33 PM
 */

// TODO: Refactor with UserRepository
object GameRepositoryService extends CommonService {

  override val symbol = Symbol("GameRepositoryService")

  registerAsRemoteActor

  def act {
    loop {
      react {
        case x =>
          GameRepository.handleMessage( x ) match {
            case message:DataRepositoryMessage => reply( message )
            case _ =>
          }
        
      }
    }
  }

}

