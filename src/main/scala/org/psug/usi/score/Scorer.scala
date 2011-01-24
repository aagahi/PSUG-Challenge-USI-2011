package org.psug.usi.score

import scala.actors._

class UserResponseAgent(val uid: Int, val scorer: Scorer) {

  def this(scorer: Scorer) = this(0, scorer)

  def ok: Int = {
    (scorer !? uid).asInstanceOf[Int]
  }

}

class Scorer(val numUsers: Int)(implicit val interval: Int) extends Actor {

  /*
   * Sorted array of scores. This array is expected to be always sorted in increasing
   * order of scores.
   */
  val scores: Array[(Int, Int)] = new Array[(Int, Int)](numUsers)

  /*
   * Index user ids within scores array. The index of this array are the user ids, elements
   * are indices in the scores array containing this users score.
   */
  val usersScoresIndex: Array[Int] = new Array[Int](numUsers)

  for (i <- 0 to numUsers - 1) { usersScoresIndex(i) = i; scores(i) = (i, 0) }

  /**
   * Reassign a score for a user id within the sorted array of scores.
   */
  def reassign(score: (Int, Int)): Int = {
    val start = usersScoresIndex(score._1) + 1
    val end = findPosition(score,start)
    if (end > start) {
      val toMove = scores.slice(start, end)
      System.arraycopy(toMove, 0, scores, start - 1, toMove.length)
      decreasePositions(start - 1, end - 1)
      usersScoresIndex(score._1) = end - 1
    }
    scores(end - 1) = score
    end - 1
  }

  /**
   * Find position of new score within scores array with dichotomic search.
   * @return the index in scores with score greater than score's value, or the
   * length of scores.
   */
  private def findPosition(score: (Int, Int), start: Int): Int = {
    var beg = start
    var end = scores.length
    var mid = (end - beg) / 2 + beg
    while(mid != end && beg != mid) {
      if(scores(mid)._2 < score._2) {
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
      usersScoresIndex(scores(i)._1) = i
  }

  def updatePosition(i: Int, score: (Int, Int)) = {
    usersScoresIndex(score._1) = i
    scores(i) = score
  }

  def act {
    loop {
      react {
        case uid: Int => {
          val (user, sc) = scores(usersScoresIndex(uid))
          val newScore = (user, sc + 1)
          reassign(newScore)
          reply(newScore._2)
        }
      }
    }
  }

  def score(uid: Int): Array[(Int, Int)] =
    scores.slice(usersScoresIndex(uid) - 10, usersScoresIndex(uid) + 11)

  override def toString = {
    "Scores: (" +
      scores.deep.mkString(",") +
      "), Users positions: (" +
      usersScoresIndex.deep.mkString(",") +
      ")"
  }

}
