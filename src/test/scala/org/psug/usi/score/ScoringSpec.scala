package org.psug.usi.score

import org.specs._

class ScoringSpec extends SpecificationWithJUnit {

  "scoring agent" should {
    
    "record correct answer as 1 increment to score for a single user" in {
      val scorer = new Scorer(1)
      val userResponse = new UserResponseAgent(scorer)
      val user = 0
      scorer.start
      userResponse.ok(user) must be_==(1)
      userResponse.ok(user) must be_==(2)
      scorer.score(user)(0) must be_==((0,2))
    }

    "update position of user and send score of users before and after" in {
      val scorer = new Scorer(3)
      val userResponse = new UserResponseAgent(scorer)
      val user = 1
      scorer.start
      userResponse.ok(user) must be_==(1)
      scorer.score(user)(2) must be_==((1,1))      
    }
    
  }
}



