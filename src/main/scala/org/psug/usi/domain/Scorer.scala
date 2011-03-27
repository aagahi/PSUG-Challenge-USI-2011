package org.psug.usi.domain

import collection.immutable.TreeSet
import collection.mutable.HashMap

/**
 * Strange structure to match:
 * https://sites.google.com/a/octo.com/challengeusi2011/l-application-de-quiz#TOC-Obtenir-le-classement-d-un-utilisat
 */
case class ListScores(mail:Array[String], scores:Array[Int], firstname:Array[String], lastname:Array[String]) {
  require(mail.size == scores.size && scores.size == firstname.size && firstname.size == lastname.size)
  
  def deepEquals(that:ListScores) : Boolean = mail.deep.equals(that.mail.deep) &&
    scores.deep.equals(that.scores.deep) &&
    firstname.deep.equals(that.firstname.deep) &&
    lastname.deep.equals(that.lastname.deep)
}

case class Ranking(score:Int, top_scores:ListScores, before:ListScores, after:ListScores) {
  def deepEquals(that:Ranking) : Boolean = score == that.score && 
  top_scores.deepEquals(that.top_scores) &&
  before.deepEquals(that.before) && 
  after.deepEquals(that.after)
}

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

class Scorer(val numUsers: Int, val sliceRange:Range = -10 to 10, val topSize:Int = 100 ) {


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

  //transform a list of UserScore to a ListScore structure
  private[this] def toListScore(scores:Traversable[UserScore]) : ListScores = {
    val s = scores.size
    val listScore = ListScores(new Array(s), new Array(s), new Array(s), new Array(s))
    var i = 0
    scores.foreach { case UserScore(user,score) =>
      listScore.mail(i) = user.mail
      listScore.scores(i) = score
      listScore.firstname(i) = user.firstname
      listScore.lastname(i) = user.lastname
      i += 1
    }
    listScore
  }
  //retrieve the Top 100. Do the calcul only one time (it has to be called at the end of game
  private[this] lazy val topPlayers : ListScores = toListScore(sortedUserScores.take(topSize))
  
  def scoreSlice(user: User):Ranking = {
    val userScore = userScoresMap( user )
    val before = toListScore((sortedUserScores until userScore).takeRight( -sliceRange.start ))
    val after = toListScore((sortedUserScores from userScore).drop(1).take( sliceRange.end ))
    Ranking(userScore.score,topPlayers,before,after)
  }

  override def toString = {
    "Scores: (" + userScoresMap.values.mkString(",") + ")"
  }

}
