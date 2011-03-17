package org.psug.usi.domain

import org.specs._

class ScoringSpec extends SpecificationWithJUnit with PerfUtilities{

  implicit val defaultInterval = 100


  def isSorted(seq : Seq[UserScore]) : Boolean = {

    seq.foldLeft( UserScore( User(0, "","","","" ), Int.MaxValue ), true ){
      ( r:(UserScore,Boolean), s:UserScore) =>
        if( s.score == r._1.score ) ( s, r._2 & ( (s.user compare r._1.user ) < 0 ) )
        else (s, r._2 & (s.score < r._1.score) )
    }._2
  }

  "scoring agent" should {
    
    "record correct answer update score on multiple responses for a single user" in {
      val scorer = new Scorer(1)
      val user = User( 0, "0","","","" )

      scorer.scoreAnwser( ScorerAnwserValue( user, 1 ) ) must be_==( UserScore( user,1) )
      scorer.scoreAnwser( ScorerAnwserValue( user, 5 ) ) must be_==(UserScore( user, 5+1 ) )
      scorer.scoreAnwser( ScorerAnwserValue( user, 0 ) ) must be_==(UserScore( user, 5+1+0 ) )
      scorer.scoreAnwser( ScorerAnwserValue( user, 5 ) ) must be_==(UserScore( user, 5+1+0+5 ) )
    }

    "users with same score must be sorted by firstname/lastname/email" in {
      val scorer = new Scorer(2)
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

      val expectedSortedList = UserScore( user1, 1 ) :: UserScore( user2, 1 ) :: UserScore( user3, 1 ) :: UserScore( user4, 1 ) :: UserScore( user5, 1 ) :: UserScore( user6, 0 )  :: Nil
      isSorted( expectedSortedList ) must be_==( true )

      scorer.scoreSlice(user1) must be_==( expectedSortedList )
      scorer.scoreSlice(user2) must be_==( expectedSortedList )
      scorer.scoreSlice(user3) must be_==( expectedSortedList )
      scorer.scoreSlice(user4) must be_==( expectedSortedList )
      scorer.scoreSlice(user5) must be_==( expectedSortedList )
      scorer.scoreSlice(user6) must be_==( expectedSortedList )

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
      val scorer = new Scorer(numberOfPlayers)

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



