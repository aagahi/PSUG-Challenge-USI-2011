package org.psug.usi.twitter

import org.specs._
import org.psug.usi.twitter.Twitter._
class TwitterSpec extends SpecificationWithJUnit {

  "twitter client" should {

    "post a message on twitter and destroy it (if it can connect)" in {
      try {
        val Some(id) = update("Notre application supporte 100000 joueurs #challengeUSI2011")
        id must be_!=(0)
        destroy(id)
      } catch {
        case e : Exception =>  skip("got exception " +e +" while trying to post to " + twitterHost+", skipping test")
      }
    }


  }

}
