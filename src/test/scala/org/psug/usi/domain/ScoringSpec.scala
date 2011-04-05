package org.psug.usi.domain

import org.specs._
import matcher.Matcher
import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner
import org.psug.usi.utils.RankingUtil._

@RunWith(classOf[JUnitSuiteRunner])
class ScoringSpec extends SpecificationWithJUnit with PerfUtilities {

  implicit val defaultInterval = 100



  "scoring agent" should {

    "record correct answer update score on multiple responses for a single user" in {
      val scorer = new Scorer(1)
      val user = User(0, "0", "", "", "")

      scorer.scoreAnwser(ScorerAnwserValue(user, 1)) must be_==(UserScore(user, 1))
      scorer.scoreAnwser(ScorerAnwserValue(user, 5)) must be_==(UserScore(user, 5 + 1))
      scorer.scoreAnwser(ScorerAnwserValue(user, 0)) must be_==(UserScore(user, 5 + 1 + 0))
      scorer.scoreAnwser(ScorerAnwserValue(user, 5)) must be_==(UserScore(user, 5 + 1 + 0 + 5))
    }

    "users with same score must be sorted by firstname/lastname/mail" in {
      val scorer = new Scorer(6, topSize = 5)
      val user1 = User(0, "A", "A", "B", "")
      val user2 = User(1, "A", "A", "C", "")
      val user3 = User(2, "A", "A", "D", "")
      val user4 = User(3, "A", "B", "E", "")
      val user5 = User(4, "B", "B", "F", "")
      val user6 = User(5, "A", "A", "A", "")


      scorer.scoreAnwser(ScorerAnwserValue(user6, 0)) must be_==(UserScore(user6, 0))
      scorer.scoreAnwser(ScorerAnwserValue(user5, 1)) must be_==(UserScore(user5, 1))
      scorer.scoreAnwser(ScorerAnwserValue(user3, 1)) must be_==(UserScore(user3, 1))
      scorer.scoreAnwser(ScorerAnwserValue(user4, 1)) must be_==(UserScore(user4, 1))
      scorer.scoreAnwser(ScorerAnwserValue(user1, 1)) must be_==(UserScore(user1, 1))
      scorer.scoreAnwser(ScorerAnwserValue(user2, 1)) must be_==(UserScore(user2, 1))

      //create the expected ranking for each user
      val top5 = ListScores(
        mail = Array("B", "C", "D", "E", "F"),
        scores = Array(1, 1, 1, 1, 1),
        firstname = Array("A", "A", "A", "A", "B"),
        lastname = Array("A", "A", "A", "B", "B")
      )

      def haveSameRankingThan(ranking: Ranking) = new Matcher[Ranking] {
        def apply(other: => Ranking) = {
          val sameScore = ranking.score == other.score
          val sameTopScores = ranking.top_scores.deepEquals(other.top_scores)
          val sameBefore = ranking.before.deepEquals(other.before)
          val sameAfter = ranking.after.deepEquals(other.after)

          def errorMessage: String =
            (if (!sameScore) "score " + other.score + " is not " + ranking.score else "") +
              (if (!sameTopScores) "top scores " + other.top_scores + " is not " + ranking.top_scores else "") +
              (if (!sameBefore) "users before " + other.before + " is not " + ranking.before else "") +
              (if (!sameAfter) "users after " + other.after + " is not " + ranking.after else "")

          (sameScore & sameTopScores & sameBefore & sameAfter
            , "ranking are identical"
            , errorMessage
            )
        }
      }

      scorer.scoreSlice(user1) must haveSameRankingThan(Ranking(1, top5,
        before = ListScores(Array(), Array(), Array(), Array()),
        after = ListScores(Array("C", "D", "E", "F", "A"), Array(1, 1, 1, 1, 0), Array("A", "A", "A", "B", "A"), Array("A", "A", "B", "B", "A"))
      ))

      scorer.scoreSlice(user2) must haveSameRankingThan(Ranking(1, top5,
        before = ListScores(Array("B"), Array(1), Array("A"), Array("A")),
        after = ListScores(Array("D", "E", "F", "A"), Array(1, 1, 1, 0), Array("A", "A", "B", "A"), Array("A", "B", "B", "A"))
      ))

      scorer.scoreSlice(user3) must haveSameRankingThan(Ranking(1, top5,
        before = ListScores(Array("B", "C"), Array(1, 1), Array("A", "A"), Array("A", "A")),
        after = ListScores(Array("E", "F", "A"), Array(1, 1, 0), Array("A", "B", "A"), Array("B", "B", "A"))
      ))

      scorer.scoreSlice(user4) must haveSameRankingThan(Ranking(1, top5,
        before = ListScores(Array("B", "C", "D"), Array(1, 1, 1), Array("A", "A", "A"), Array("A", "A", "A")),
        after = ListScores(Array("F", "A"), Array(1, 0), Array("B", "A"), Array("B", "A"))
      ))

      scorer.scoreSlice(user5) must haveSameRankingThan(Ranking(1, top5,
        before = ListScores(Array("B", "C", "D", "E"), Array(1, 1, 1, 1), Array("A", "A", "A", "A"), Array("A", "A", "A", "B")),
        after = ListScores(Array("A"), Array(0), Array("A"), Array("A"))
      ))

      scorer.scoreSlice(user6) must haveSameRankingThan(Ranking(0, top5,
        before = ListScores(Array("B", "C", "D", "E", "F"), Array(1, 1, 1, 1, 1), Array("A", "A", "A", "A", "B"), Array("A", "A", "A", "B", "B")),
        after = ListScores(Array(), Array(), Array(), Array())
      ))
    }


    "compute accurate user score for large number of player" in {
      val numberOfPlayers = 4000
      val scorer = new Scorer(numberOfPlayers)

      val users = 1 until numberOfPlayers map (i => User(i, i.toString, "f" + i, "l" + 1, "e" + i))
      val numResponse = 10
      1 to numResponse foreach {
        i =>
          for (user <- users) {
            if (user.id % 2 == 0) scorer.scoreAnwser(ScorerAnwserValue(user, 1))
            else scorer.scoreAnwser(ScorerAnwserValue(user, 0))
          }
      }

      for (user <- users) {
        val score = scorer.userScore(user)
        if (user.id % 2 == 0) score must be(numResponse)
        else score must be(0)
      }
    }

    "produce sorted scoreSlice for a very large number of player" in {
      val numberOfPlayers = 4000
      val scorer = new Scorer(numberOfPlayers, topSize = 100)

      val users = 1 until numberOfPlayers map (i => User(i, (i % 8).toString, (i % 16).toString, i.toString, ""))
      for (user <- users) {
        if (user.id % 1000 == 0) scorer.scoreAnwser(ScorerAnwserValue(user, 1))
        else if (user.id % 10 == 0) scorer.scoreAnwser(ScorerAnwserValue(user, 1))
        else if (user.id % 4 == 0) scorer.scoreAnwser(ScorerAnwserValue(user, 1))
        else if (user.id % 2 == 0) scorer.scoreAnwser(ScorerAnwserValue(user, 1))
        else scorer.scoreAnwser(ScorerAnwserValue(user, 0))
      }


      for (user <- users) {
        val slice = scorer.scoreSlice(user)
        isSorted(slice) must beTrue
      }
    }

  }

}



