package org.psug.usi.score

import org.specs._

class ScoringSpec extends SpecificationWithJUnit {

  implicit val defaultInterval = 100

  "scoring agent" should {
    
    "record correct answer as 1 increment to score for a single user" in {
      val scorer = new Scorer(1)
      val userResponse = new UserResponseAgent(scorer)
      val user = 0
      scorer.start
      userResponse.ok must be_==(1)
      userResponse.ok must be_==(2)
      scorer.score(user)(0) must be_==((0,2))
    }

    "update position of user and send score of users before and after" in {
      val scorer = new Scorer(3)
      val user = 1
      val userResponse = new UserResponseAgent(user,scorer)
      scorer.start
      userResponse.ok must be_==(1)
      scorer.score(user)(2) must be_==((1,1))      
    }
    
    "send score of all users within a specific given interval" in {
      val numberOfPlayers = 1000
      val scorer = new Scorer(numberOfPlayers)(10)
      val users : Array[UserResponseAgent] = new Array[UserResponseAgent](numberOfPlayers)
      scorer.start
      scorer.score(900).length must be_==(21)
      scorer.score(999).length must be_==(11)
      scorer.score(0).length must be_==(11)
    }
    
    "compute score for a very large number of users" in {
      val numberOfPlayers = 4000
      val scorer = new Scorer(numberOfPlayers)(10)
      val users : Array[UserResponseAgent] = new Array[UserResponseAgent](numberOfPlayers)
      scorer.start
      for(i <- 0 to numberOfPlayers-1) { users(i) = new UserResponseAgent(i,scorer) }
      for(i <- 0 to numberOfPlayers-1) { 
	if(i % 1000 == 0) users(i).ok
	if(i % 10 == 0) users(i).ok
	if(i % 4  == 0) users(i).ok
	if(i % 2  == 0) users(i).ok 
      }
      val sc = scorer.score(1000)
       sc(10) must be_==((1000,4))
    }
  }
}



