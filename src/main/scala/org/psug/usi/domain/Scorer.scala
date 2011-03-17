package org.psug.usi.domain

import collection.immutable.TreeSet
import collection.mutable.HashMap

case class UserScore( user:User, score:Int ) extends Ordered[UserScore] {
  def update( answerValue:Int ) = {
    UserScore( user, score + answerValue )
  }
  def compare(that: UserScore) = {
    val c1 = that.score compare score
    if( c1 == 0 ) that.user compare user
    else c1
  }
}
case class ScorerAnwserValue( user:User, answerValue:Int ) // 0 mean wrong answer

class Scorer(val numUsers: Int, val sliceRange:Range = -10 to 10 ) {


  var sortedUserScores = new TreeSet[UserScore]
  val userScoresMap = new HashMap[User,UserScore]

  def scoreAnwser( scorerAnwserValue:ScorerAnwserValue ) = {
    val user = scorerAnwserValue.user
    val userScore = userScoresMap.getOrElse( user, UserScore( user, 0 ) )

    val newScore = userScore.update( scorerAnwserValue.answerValue )
    userScoresMap( user ) = newScore
    sortedUserScores = sortedUserScores + newScore
    newScore
  }

  def userScore( user:User ) = userScoresMap(user).score

  def scoreSlice(user: User):List[UserScore] = {
    val userScore = userScoresMap( user )
    (sortedUserScores until userScore).takeRight( -sliceRange.start ).toList ::: (sortedUserScores from userScore).take( sliceRange.end ).toList
  }

  override def toString = {
    "Scores: (" + userScoresMap.values.mkString(",") + ")"
  }

}
