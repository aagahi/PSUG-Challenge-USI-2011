package org.psug.usi.score

import scala.actors._

case class UserScore(val id: Int, val score : Int)

class UserResponseAgent(val scorer : Scorer) {

  def ok (userId : Int) : Int = {
    (scorer !? userId).asInstanceOf[Int]
  }
}

class Scorer(val numUsers : Int) extends Actor {
  
  var scores : Array[(Int,Int)] = new Array[(Int,Int)](numUsers)
  var usersScoresIndex  : Array[Int] = new Array[Int](numUsers)

  for(i <- 0 to numUsers -1) { usersScoresIndex(i) = i; scores(i) = (i,0) }

  def act {
    loop {
      react {
	case uid : Int => {
	  val (_,sc) = scores(usersScoresIndex(uid))
	  scores(usersScoresIndex(uid)) = (uid,sc + 1)
	  reply(sc + 1) 
	}
      }
    }
  }

  def score(uid : Int) : Array[(Int,Int)] = 
    scores
    
}
