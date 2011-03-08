package org.psug.usi.twitter


import org.specs._
import org.psug.usi.twitter.Twitter._

class TwitterSpec  extends SpecificationWithJUnit {


  "twitter client" should {


    /**
     * This test post a message on twitter. If it fails, check if the last message in http://twitter.com/#!/psugusi2011 is not "Hello World"
     * Otherwise you should delete it before running the test
     */
    "post a message on twitter and destroy it" in {

      val Some( id ) = update("Hello World")
      id  must be_!=( 0 )
      destroy( id )

    }
    



  }
  
}
