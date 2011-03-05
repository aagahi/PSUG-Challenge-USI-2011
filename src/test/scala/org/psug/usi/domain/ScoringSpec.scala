package org.psug.usi.domain

import org.specs._

class ScoringSpec extends SpecificationWithJUnit with PerfUtilities{

  implicit val defaultInterval = 100

  def score( scorer:Scorer, answerValue:Int, userId:Int = 0 ):UserScore = {
    ( scorer !? UserResponse( userId, answerValue ) ).asInstanceOf[UserScore]
  }

  def isSorted(seq : Seq[UserScore]) : Boolean = {
    seq.foldLeft( 0, true )( ( r:(Int,Boolean), s:UserScore) => ( s.score, r._2 & (s.score >= r._1) ) )._2
  }

  "scoring agent" should {
    
    "record correct answer update score as (ansserValue + bonus) for a single user" in {
      val scorer = new Scorer(1)
      val userId = 0

      score( scorer, 1, userId ) must be_==(UserScore(0,1,1))
      score( scorer, 5, userId ) must be_==(UserScore(0,1+5+1,2))
      score( scorer, 0, userId ) must be_==(UserScore(0,1+5+1,0))
      score( scorer, 1, userId ) must be_==(UserScore(0,1+5+1+1,1))
      score( scorer, 3, userId ) must be_==(UserScore(0,1+5+1+1+3+1,2))


      scorer.score(userId)(0) must be_==(UserScore(0,1+5+1+1+3+1,2))
    }

    "update position of user and send score of users before and after" in {
      val scorer = new Scorer(3)
      val userId = 1

      score( scorer, 1, userId ) must be_==( UserScore(1,1,1) )
      scorer.score(userId)(2) must be_==(UserScore(1,1,1))
    }
    
    "send score of all users within a specific given interval" in {
      val numberOfPlayers = 1000
      val scorer = new Scorer(numberOfPlayers, -50 to 50)

      scorer.score(900).length must be_==(50 * 2)
      scorer.score(999).length must be_==(50 + 1)
      scorer.score(0).length must be_==(50)
    }
    
    "scorer always store scores in a sorted array" in {
      val numberOfPlayers = 1000
      val scorer = new Scorer(numberOfPlayers)
      simulateAnswers( 10, 0.4, scorer, 1 until numberOfPlayers )
      isSorted(scorer.scores) must be_==(true)
    }


    "compute score for a very large number of users" in {
      val numberOfPlayers = 4000
      val scorer = new Scorer(numberOfPlayers)

      for(i <- 0 until numberOfPlayers ){
        if(i % 1000 == 0) score( scorer, 1, i )
        if(i % 10 == 0) score( scorer, 1, i )
        if(i % 4  == 0) score( scorer, 1, i )
        if(i % 2  == 0) score( scorer, 1, i )
      }

      val sc = scorer.score(1000)
      sc(10) must be_==(UserScore(1000,4+(0+1+2+3),4))
    }
  }

}



