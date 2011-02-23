package org.psug.usi.domain

import org.specs._

class ScoringSpec extends SpecificationWithJUnit with PerfUtilities{

  implicit val defaultInterval = 100

  def isSorted(seq : Seq[UserScore]) : Boolean = {
    seq.foldLeft( 0, true )( ( r:(Int,Boolean), s:UserScore) => ( s.score, r._2 & (s.score >= r._1) ) )._2
  }

  "scoring agent" should {
    
    "record correct answer as 1 increment to score for a single user" in {
      val scorer = new Scorer(1)
      val userResponse = new UserResponseAgent(scorer)
      val score = 0
      userResponse.ok must be_==(UserScore(0,1,1))
      userResponse.ok must be_==(UserScore(0,3,2))
      userResponse.ko must be_==(UserScore(0,3,0))
      userResponse.ok must be_==(UserScore(0,4,1))
      userResponse.ok must be_==(UserScore(0,6,2))
      scorer.score(score)(0) must be_==(UserScore(0,6,2))
    }

    "update position of user and send score of users before and after" in {
      val scorer = new Scorer(3)
      val user = 1
      val userResponse = new UserResponseAgent(user,scorer)
      userResponse.ok must be_==( UserScore(1,1,1) )
      scorer.score(user)(2) must be_==(UserScore(1,1,1))
    }
    
    "send score of all users within a specific given interval" in {
      val numberOfPlayers = 1000
      val scorer = new Scorer(numberOfPlayers, -50 to 50)
      val users : Array[UserResponseAgent] = new Array[UserResponseAgent](numberOfPlayers)
      scorer.score(900).length must be_==(50 * 2)
      scorer.score(999).length must be_==(50 + 1)
      scorer.score(0).length must be_==(50)
    }
    
    "scorer always store scores in a sorted array" in {
      val numberOfPlayers = 1000
      val scorer = new Scorer(numberOfPlayers)
      val users : Array[UserResponseAgent] = new Array[UserResponseAgent](numberOfPlayers)
      for(i <- 0 to numberOfPlayers-1) { users(i) = new UserResponseAgent(i,scorer) }
      simulateAnswers(10,0.4,users)
      isSorted(scorer.scores) must be_==(true)
    }
    
    "compute score for a very large number of users" in {
      val numberOfPlayers = 4000
      val scorer = new Scorer(numberOfPlayers)
      val users : Array[UserResponseAgent] = new Array[UserResponseAgent](numberOfPlayers)
      for(i <- 0 to numberOfPlayers-1) { users(i) = new UserResponseAgent(i,scorer) }
      for(i <- 0 to numberOfPlayers-1) { 
	    if(i % 1000 == 0) users(i).ok
	    if(i % 10 == 0) users(i).ok
	    if(i % 4  == 0) users(i).ok
	    if(i % 2  == 0) users(i).ok
      }
      val sc = scorer.score(1000)
      sc(10) must be_==(UserScore(1000,4+(0+1+2+3),4))
    }
  }
}



