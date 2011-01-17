package org.psug.usi.score

import org.specs._

class ScoringSpec extends SpecificationWithJUnit {

  "scoring agent" should {
    
    "record correct answer as 1 increment to score for a single user" in {
      val scorer = new Scorer
      val userResponse = new UserResponseAgent(scorer)
      val user = 0
      scorer.start
      userResponse.ok(user) must be_==(1)
      userResponse.ok(user) must be_==(2)
      scorer.score(user) must be_==(2)
    }

  }
}



