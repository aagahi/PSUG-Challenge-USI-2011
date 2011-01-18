package org.psug.usi.score

import scala.actors._

case class UserScore(val id: Int, val score : Int)

class UserResponseAgent(val scorer : Scorer) {

  def ok (userId : Int) : Int = {
    (scorer !? UserScore(userId,1)).asInstanceOf[Int]
  }
}

class Scorer(val numUsers : Int) extends Actor {
  
  var scores : Array[(Int,Int)] = new Array[(Int,Int)](numUsers)
  var usersScoresIndex  : Array[Int] = new Array[Int](numUsers)

  for(i <- 0 to numUsers -1) { usersScoresIndex(i) = i; scores(i) = (i,0) }

  def act {
    loop {
      react {
	case UserScore(uid,score) => {
	  val (_,sc) = scores(usersScoresIndex(uid))
	  scores(usersScoresIndex(uid)) = (uid,sc + score)
	  reply(sc + score) 
	}
      }
    }
  }

  def score(uid : Int) : Array[(Int,Int)] = 
    scores
    
}
