package org.psug.usi.domain

import collection.mutable.HashMap
import java.util.Properties
import java.io._

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

class Scorer(val sliceRange: Range = -5 to 5, val topSize: Int = 100) {

  val properties = new Properties()
  properties.load( getClass.getResourceAsStream( "/configuration.properties" ) )
  val scorerFolder = new File( properties.getProperty("scorer.folder") )
  scorerFolder.mkdirs

  
  val userIdScoresMap = new HashMap[Int, UserScore]

  def scoreAnwser(scorerAnwserValue: ScorerAnwserValue) = {

    val user = scorerAnwserValue.user
    val userScore = userIdScoresMap.getOrElse(user.id, UserScore(user, 0))
    val newScore = userScore.update(scorerAnwserValue.answerValue)
    userIdScoresMap(user.id) = newScore

    newScore
  }

  def userScore(userId: Int) = userIdScoresMap.get(userId) match {
    case Some( userScore ) => userScore.score
    case None => 0
  }

  //retrieve the Top 100. Do the calcul only one time (it has to be called at the end of game
  private[this] lazy val topPlayers: ListScoresVO = ListScoresVO(sortedUserScores.take(topSize))


  def scoreSlice(user: User): RankingVO = {
    val indexOfUser: Int = sortedUserScores.indexWhere( _.user.id == user.id )
    val userScore = sortedUserScores(indexOfUser)
    val begin = if (indexOfUser + sliceRange.start < 0) 0 else (indexOfUser + sliceRange.start)
    val numUsers = sortedUserScores.size
    val end = if (numUsers < indexOfUser + sliceRange.end+1) numUsers else indexOfUser + sliceRange.end +1
    val before = ListScoresVO(sortedUserScores.slice(begin, indexOfUser))
    val after = ListScoresVO(sortedUserScores.slice(indexOfUser + 1, end))
    RankingVO(userScore.score, topPlayers, before, after)
  }

  override def toString = {
    "Scores: (" + userIdScoresMap.values.mkString(",") + ")"
  }

  private var _sortedUserScores:List[UserScore] = null
  def sortedUserScores = {
    if( _sortedUserScores == null ) _sortedUserScores = userIdScoresMap.values.toList.sorted
    _sortedUserScores
  }


  def save( gameId:Int ){
    val fos = new BufferedOutputStream( new FileOutputStream( new File( scorerFolder, gameId.toString ) ) )
    val out = new ObjectOutputStream(fos)
    out.writeObject( sortedUserScores )
    fos.close

  }

  def load( gameId:Int ){
    val scorerFile = new File( scorerFolder, gameId.toString )
    if( scorerFile.exists ){
      val fis = new BufferedInputStream( new FileInputStream( scorerFile ) )
      val in = new ObjectInputStream(fis)
      _sortedUserScores = in.readObject().asInstanceOf[List[UserScore]]
      fis.close
    }

  }

}
