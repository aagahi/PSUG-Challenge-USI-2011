package org.psug.usi.score

import scala.actors._

case class UserScore(val id: Int, val score : Int)

class UserResponseAgent(val scorer : Scorer) {

  def ok (userId : Int) : Int = {
    (scorer !? UserScore(userId,1)).asInstanceOf[Int]
  }
}

class Scorer extends Actor {
  
  var scores : Array[Int] = new Array[Int](1)

  def act {
    loop {
      react {
	case UserScore(uid,score) => scores(uid) = scores(uid) + score; reply(scores(uid))
      }
    }
  }

  def score(uid : Int) : Int = scores(uid)
}
