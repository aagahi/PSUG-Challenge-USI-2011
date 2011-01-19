package org.psug.usi.score

import scala.actors._


class UserResponseAgent(val uid : Int, val scorer : Scorer) {
  
  def this(scorer : Scorer) = this(0,scorer)

  def ok : Int = {
    (scorer !? uid).asInstanceOf[Int]
  }

}

class Scorer(val numUsers : Int)(implicit val interval : Int) extends Actor {
  
  /*
   * Sorted array of scores. This array is expected to be always sorted in increasing
   * order of scores.
   */ 
  var scores : Array[(Int,Int)] = new Array[(Int,Int)](numUsers)

  /*
   * Index user ids within scores array. The index of this array are the user ids, elements
   * are indices in the scores array containing this users score.
   */ 
  var usersScoresIndex  : Array[Int] = new Array[Int](numUsers)

  for(i <- 0 to numUsers -1) { usersScoresIndex(i) = i; scores(i) = (i,0) }

  /**
   * Reassign a score for a user id within the sorted array of scores.
   */
  def reassign(score : (Int,Int)) : Int = {
    var start = usersScoresIndex(score._1) +1
    var end = usersScoresIndex(score._1) +1
    while(scores.length > end && scores(end)._2 < score._2) {
      end = end + 1
    } 
    if(end > start) {
      val toMove = scores.slice(start,end)
      System.arraycopy(toMove,0,scores,start-1,toMove.length)
      decreasePositions(start-1,end-1)
      usersScoresIndex(score._1) = end-1
    }
    scores(end-1) = score
    end-1
  }

  /**
   * Decrement by one the position of each user within the given range (to not included).
   */
  def decreasePositions(beg : Int, end : Int) = {
    for(i <- beg to end-1)
      usersScoresIndex(scores(i)._1) = i
  }

  def updatePosition(i : Int, score : (Int,Int)) = {
    usersScoresIndex(score._1) = i
    scores(i) = score
  }

  def act {
    loop {
      react {
	case uid : Int => {
	  val (user,sc) = scores(usersScoresIndex(uid))
	  val newScore = (user,sc+1)
	  reassign(newScore)
	  reply(newScore._2) 
	}
      }
    }
  }

  def score(uid : Int) : Array[(Int,Int)] = 
    scores.slice(usersScoresIndex(uid)-10,usersScoresIndex(uid)+11)

  
  override def toString = {
    "Scores: (" + 
    scores.deep.mkString(",") + 
    "), Users positions: (" + 
    usersScoresIndex.deep.mkString(",") + 
    ")"
  }

}
