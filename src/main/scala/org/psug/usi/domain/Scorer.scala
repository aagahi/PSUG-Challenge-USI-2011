package org.psug.usi.domain

import collection.immutable.TreeSet
import collection.mutable.HashMap

/**
 * Strange structure to match:
 * https://sites.google.com/a/octo.com/challengeusi2011/l-application-de-quiz#TOC-Obtenir-le-classement-d-un-utilisat
 */
object ListScoresVO {
  def apply( scores: Traversable[UserScore] ):ListScoresVO = {
    val s = scores.size
    val listScore = ListScoresVO(new Array(s), new Array(s), new Array(s), new Array(s))
    var i = 0
    scores.foreach {
      case UserScore(user, score) =>

        listScore.mail(i) = user.mail
        listScore.scores(i) = score
        listScore.firstname(i) = user.firstname
        listScore.lastname(i) = user.lastname
        i += 1
    }
    listScore
  }
}


case class ListScoresVO(mail: Array[String], scores: Array[Int], firstname: Array[String], lastname: Array[String]) {
  require(mail.size == scores.size && scores.size == firstname.size && firstname.size == lastname.size)

  def deepEquals(that: ListScoresVO): Boolean = mail.deep.equals(that.mail.deep) &&
    scores.deep.equals(that.scores.deep) &&
    firstname.deep.equals(that.firstname.deep) &&
    lastname.deep.equals(that.lastname.deep)

  override def toString: String = {
    "ListScoresVO(" + mail.deep.mkString("[",",","]") + ", "
                    scores.deep.mkString("[",",","]") + ", "
                    firstname.deep.mkString("[",",","]") + ", "
                    lastname.deep.mkString("[",",","]") + ")"
  }
}

case class RankingVO(score: Int, top_scores: ListScoresVO, before: ListScoresVO, after: ListScoresVO) {
  def deepEquals(that: RankingVO): Boolean = score == that.score &&
    top_scores.deepEquals(that.top_scores) &&
    before.deepEquals(that.before) &&
    after.deepEquals(that.after)

}

case class UserScore(user: User, score: Int) extends Ordered[UserScore] {
  def update(answerValue: Int) = {
    UserScore(user, score + answerValue)
  }

  override def hashCode() = {
    user.id.hashCode
  }

  def compare(that: UserScore) = {
    val c1 = that.score compare score
    if (c1 == 0) that.user compare user
    else c1
  }
}

case class ScorerAnwserValue(user: User, answerValue: Int)

// 0 mean wrong answer

class Scorer(val numUsers: Int, val sliceRange: Range = -10 to 10, val topSize: Int = 100) {

  var sortedUserScores = new TreeSet[UserScore]
  val userIdScoresMap = new HashMap[Int, UserScore]

  def scoreAnwser(scorerAnwserValue: ScorerAnwserValue) = {
    val user = scorerAnwserValue.user
    val userScore = userIdScoresMap.getOrElse(user.id, UserScore(user, 0))
    sortedUserScores = sortedUserScores - userScore

    val newScore = userScore.update(scorerAnwserValue.answerValue)

    userIdScoresMap(user.id) = newScore
    sortedUserScores = sortedUserScores + newScore
    newScore
  }

  def userScore(userId: Int) = userIdScoresMap.get(userId) match {
    case Some( userScore ) => userScore.score
    case None => 0
  }

  //retrieve the Top 100. Do the calcul only one time (it has to be called at the end of game
  private[this] lazy val topPlayers: ListScoresVO = ListScoresVO(sortedUserScores.take(topSize))

  /**
   * a lazily evaluated array filled from the sorted set of users.
   * Allows more efficient extraction of slices and is only computed at first call.
   */
  private[this] lazy val arrayOfScores: Array[UserScore] = {
    val a: Array[UserScore] = new Array[UserScore](sortedUserScores.size)
    sortedUserScores.copyToArray(a)
    a
  }

  def scoreSlice(user: User): RankingVO = {
    val userScore = userIdScoresMap(user.id)
    val indexOfUser: Int = arrayOfScores.indexOf(userScore)
    val begin = if (indexOfUser + sliceRange.start < 0) 0 else (indexOfUser + sliceRange.start)
    val end = if (numUsers < indexOfUser + sliceRange.end) numUsers else indexOfUser + sliceRange.end
    val before = ListScoresVO(arrayOfScores.slice(begin, indexOfUser))
    val after = ListScoresVO(arrayOfScores.slice(indexOfUser + 1, end))
    RankingVO(userScore.score, topPlayers, before, after)
  }

  override def toString = {
    "Scores: (" + userIdScoresMap.values.mkString(",") + ")"
  }

}
