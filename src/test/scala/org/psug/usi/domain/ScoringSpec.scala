package org.psug.usi.domain

import org.specs._
import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner

@RunWith(classOf[JUnitSuiteRunner])
class ScoringSpec extends SpecificationWithJUnit with PerfUtilities{

  implicit val defaultInterval = 100


  def isSorted(r : Ranking) : Boolean = {

    def isSorted(l:ListScores) : Boolean = {
      var i = 0
      var res = true
      while(res && i < l.scores.size-1) {
        res = {
          if(l.scores(i) > l.scores(i+1)) true
          else if(l.scores(i) < l.scores(i+1)) false 
          else {
            if(l.firstname(i) < l.firstname(i+1)) true
            else if(l.firstname(i) > l.firstname(i+1)) false
            else {
              if(l.lastname(i) < l.lastname(i+1)) true
              else if(l.lastname(i) > l.lastname(i+1)) false
              else l.mail(i) <= l.mail(i+1)
            }
          }
        }
        i += 1
      }
      res
    }
    isSorted(r.top_scores) && 
    isSorted(r.before) && 
    isSorted(r.after) && {
      if(r.before.scores.size > 0) r.before.scores(r.before.scores.size-1) >= r.score
      else true
    } && {
      if(r.after.scores.size > 0) r.after.scores(0) <= r.score
      else true
    }
  }

  "scoring agent" should {
    
    "record correct answer update score on multiple responses for a single user" in {
      val scorer = new Scorer(1)
      val user = User( 0, "0","","","" )

      scorer.scoreAnwser( ScorerAnwserValue( user, 1 ) ) must be_==(UserScore( user, 1) )
      scorer.scoreAnwser( ScorerAnwserValue( user, 5 ) ) must be_==(UserScore( user, 5+1 ) )
      scorer.scoreAnwser( ScorerAnwserValue( user, 0 ) ) must be_==(UserScore( user, 5+1+0 ) )
      scorer.scoreAnwser( ScorerAnwserValue( user, 5 ) ) must be_==(UserScore( user, 5+1+0+5 ) )
    }

    "users with same score must be sorted by firstname/lastname/mail" in {
      val scorer = new Scorer(2, topSize = 5)
      val user1 = User( 0, "A","A","B", "" )
      val user2 = User( 1, "A","A","C", "" )
      val user3 = User( 2, "A","A","D", "" )
      val user4 = User( 3, "A","B","E", "" )
      val user5 = User( 4, "B","B","F", "" )
      val user6 = User( 5, "A","A","A", "" )


      scorer.scoreAnwser( ScorerAnwserValue( user6, 0 ) ) must be_==( UserScore( user6, 0 ) )
      scorer.scoreAnwser( ScorerAnwserValue( user5, 1 ) ) must be_==( UserScore( user5, 1 ) )
      scorer.scoreAnwser( ScorerAnwserValue( user3, 1 ) ) must be_==( UserScore( user3, 1 ) )
      scorer.scoreAnwser( ScorerAnwserValue( user4, 1 ) ) must be_==( UserScore( user4, 1 ) )
      scorer.scoreAnwser( ScorerAnwserValue( user1, 1 ) ) must be_==( UserScore( user1, 1 ) )
      scorer.scoreAnwser( ScorerAnwserValue( user2, 1 ) ) must be_==( UserScore( user2, 1 ) )

      //create the expected ranking for each user
      val top5 = ListScores(
        mail = Array("B", "C", "D", "E", "F"),
        scores = Array(1,1,1,1,1),
        firstname = Array("A", "A", "A", "A", "B"),
        lastname = Array("A", "A", "A", "B", "B")
      )

      scorer.scoreSlice(user1).deepEquals( Ranking(1, top5,
        before = ListScores(Array(),Array(),Array(),Array()),
        after  = ListScores(Array("C","D","E","F","A"),Array(1,1,1,1,0),Array("A","A","A","B","A"),Array("A","A","B","B","A"))
      )) must be_==(true)
      scorer.scoreSlice(user2).deepEquals( Ranking(1, top5,
        before = ListScores(Array("B"),Array(1),Array("A"),Array("A")),
        after  = ListScores(Array("D","E","F","A"),Array(1,1,1,0),Array("A","A","B","A"),Array("A","B","B","A"))
      ) ) must be_==(true)
      scorer.scoreSlice(user3).deepEquals( Ranking(1, top5,
        before = ListScores(Array("B", "C"),Array(1,1),Array("A","A"),Array("A","A")),
        after  = ListScores(Array("E","F","A"),Array(1,1,0),Array("A","B","A"),Array("B","B","A"))
      ) ) must be_==(true)
      scorer.scoreSlice(user4).deepEquals(Ranking(1, top5,
        before = ListScores(Array("B","C","D"),Array(1,1,1),Array("A","A","A"),Array("A","A","A")),
        after  = ListScores( Array("F","A"),Array(1,0),Array("B", "A"),Array("B","A"))
      ) ) must be_==(true)
      scorer.scoreSlice(user5).deepEquals( Ranking(1, top5,
        before = ListScores(Array("B","C","D","E"),Array(1,1,1,1),Array("A","A","A","A"),Array("A","A","A","B")),
        after  = ListScores( Array("A"),Array(0),Array("A"),Array("A"))
      ) ) must be_==(true)
      scorer.scoreSlice(user6).deepEquals( Ranking(0, top5,
        before = ListScores(Array("B","C","D","E","F"),Array(1,1,1,1,1),Array("A","A","A","A","B"),Array("A","A","A","B","B")),
        after  = ListScores( Array(),Array(),Array(),Array())
      ) ) must be_==(true)
    }


    "compute accurate user score for large number of player" in {
      val numberOfPlayers = 4000
      val scorer = new Scorer(numberOfPlayers)

      val users  = 1 until numberOfPlayers map( i=> User( i, i.toString, "", "", "" ) )
      val numResponse = 10
      1 to numResponse foreach{
        i =>
        for( user <- users ){
          if( user.id % 2  == 0 ) scorer.scoreAnwser( ScorerAnwserValue( user, 1 ) )
          else scorer.scoreAnwser( ScorerAnwserValue( user, 0 ) )
        }
      }

      for( user <- users ){
        val score = scorer.userScore( user )
        if( user.id % 2  == 0 ) score must be( numResponse )
        else score must be( 0 )
      }
    }


    "produce sorted scoreSlice for a very large number of player" in {
      val numberOfPlayers = 4000
      val scorer = new Scorer(numberOfPlayers, topSize = 100)

      val users  = 1 until numberOfPlayers map( i=> User( i, (i%8).toString, (i%16).toString, i.toString, "" ) )
      for( user <- users ){
        if( user.id % 1000 == 0) scorer.scoreAnwser( ScorerAnwserValue( user, 1 ) )
        else if( user.id % 10 == 0) scorer.scoreAnwser( ScorerAnwserValue( user, 1 ) )
        else if( user.id % 4  == 0) scorer.scoreAnwser( ScorerAnwserValue( user, 1 ) )
        else if( user.id % 2  == 0) scorer.scoreAnwser( ScorerAnwserValue( user, 1 ) )
        else scorer.scoreAnwser( ScorerAnwserValue( user, 0 ) )
      }


      for( user <- users ){
        val slice = scorer.scoreSlice(user)
        isSorted( slice ) must beTrue
      }
    }

  }

}



