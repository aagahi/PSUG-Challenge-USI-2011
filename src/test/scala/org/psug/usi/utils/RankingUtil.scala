package org.psug.usi.utils

import org.psug.usi.domain.{ListScoresVO, RankingVO}

/**
 * User: alag
 * Date: 4/5/11
 * Time: 7:20 PM
 */

object  RankingUtil {

  def isSorted(l: ListScoresVO): Boolean = {
    var i = 0
    var res = true
    while (res && i < l.scores.size - 1) {
      res = {
        if (l.scores(i) > l.scores(i + 1)) true
        else if (l.scores(i) < l.scores(i + 1)) false
        else {
          if (l.lastname(i) < l.lastname(i + 1)) true
          else if (l.lastname(i) > l.lastname(i + 1)) false
          else {
            if (l.firstname(i) < l.firstname(i + 1)) true
            else if (l.firstname(i) > l.firstname(i + 1)) false
            else l.mail(i) <= l.mail(i + 1)
          }
        }
      }
      i += 1
    }
    res
  }


  def isSorted(r: RankingVO): Boolean = {

    isSorted(r.top_scores) &&
      isSorted(r.before) &&
      isSorted(r.after) && {
      if (r.before.scores.size > 0) r.before.scores(r.before.scores.size - 1) >= r.score
      else true
    } && {
      if (r.after.scores.size > 0) r.after.scores(0) <= r.score
      else true
    }
  }
}