package org.psug.usi.domain

import scala.actors._



case class UserScore( userId:Int, score:Int, bonus:Int ) {
    def update( answerValue:Int ) = {
        if( answerValue > 0 ) UserScore( userId, score + answerValue + bonus, bonus+1 )
        else UserScore( userId, score, answerValue )
    }
}
case class ScorerAnwserValue( userId:Int, answerValue:Int ) // 0 mean wrong answer

class Scorer(val numUsers: Int, sliceRange:Range = -10 to 10 ) extends Actor {

  start
  
  /*
   * Sorted array of scores. This array is expected to be always sorted in increasing
   * order of scores.
   */
  val scores: Array[UserScore] = new Array[UserScore](numUsers)

  /*
   * Index user ids within scores array. The index of this array are the user ids, elements
   * are indices in the scores array containing this users scoreSlice.
   */
  val usersScoresIndex: Array[Int] = new Array[Int](numUsers)

  for (i <- 0 to numUsers - 1) { usersScoresIndex(i) = i; scores(i) = UserScore(i, 0, 0) }

  /**
   * Reassign a scoreSlice for a user id within the sorted array of scores.
   */
  def reassign(score: UserScore): Int = {
    val start = usersScoresIndex(score.userId) + 1
    val end = findPosition(score,start)
    if (end > start) {
      val toMove = scores.slice(start, end)
      System.arraycopy(toMove, 0, scores, start - 1, toMove.length)
      decreasePositions(start - 1, end - 1)
      usersScoresIndex(score.userId) = end - 1
    }
    scores(end - 1) = score
    end - 1
  }

  /**
   * Find position of new scoreSlice within scores array with dichotomic search.
   * @return the index in scores with scoreSlice greater than scoreSlice's value, or the
   * length of scores.
   */
  private def findPosition(score: UserScore, start: Int): Int = {
    var beg = start
    var end = scores.length
    var mid = (end - beg) / 2 + beg
    while(mid != end && beg != mid) {
      if(scores(mid).score < score.score) {
	beg = mid
      } else {
	end = mid
      }
      mid = (end - beg) / 2 + beg
    }
    end
  }

  /**
   * Decrement by one the position of each user within the given range (to not included).
   */
  def decreasePositions(beg: Int, end: Int) = {
    for (i <- beg to end - 1)
      usersScoresIndex(scores(i).userId) = i
  }

  def updatePosition(i: Int, score: UserScore) = {
    usersScoresIndex(score.userId) = i
    scores(i) = score
  }

  def act {
    loop {
      react {
        case ScorerAnwserValue( uid, answserValue ) =>
          val userScore = scores(usersScoresIndex(uid))
          val newScore = userScore.update( answserValue )
          reassign(newScore)
          reply( newScore )
      }
    }
  }

  def scoreSlice(userId: Int): Array[UserScore] =
    scores.slice( usersScoresIndex(userId) + sliceRange.start, usersScoresIndex(userId) + sliceRange.end )

  override def toString = {
    "Scores: (" +
      scores.deep.mkString(",") +
      "), Users positions: (" +
      usersScoresIndex.deep.mkString(",") +
      ")"
  }

}
