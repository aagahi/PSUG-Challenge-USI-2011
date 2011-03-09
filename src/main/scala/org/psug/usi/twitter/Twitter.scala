package org.psug.usi.twitter

import org.scribe.builder.ServiceBuilder
import org.scribe.builder.api.TwitterApi
import org.scribe.model.{Verb, OAuthRequest, Token, Verifier}
import java.util.Scanner

import net.liftweb.json.JsonAST.{JInt, JField}
import net.liftweb.json.JsonParser._

/**
 * User: alag
 * Date: 3/8/11
 * Time: 10:54 PM
 */


/**
 * Use Twitter main program to setup the accessToken (if access token is set and test ok, you dont need to run the program
 */
object Twitter {
  val secretKey = "Aqzq8pi7KdbDdtehXr2BP0aZzo2nCalGmJb1zYirac"
  val apiKey = "1dUWI7wRZ3NzZdlQuQ6w"
  val service = new ServiceBuilder().provider(classOf[TwitterApi]).apiKey(apiKey).apiSecret(secretKey).build()

  // use the main program below to get the 2 value (access, secret)
  val accessToken = new Token( "261597183-rQQluvtmsGWsg65o06ZeziqBFs4IJAf3MS5g1KMZ", "Z74iH40Qvjt6ODGdRbGOG9luiEI2TPg7wl6Ib2vPFs" )

  def main(args: Array[String]) {
    val requestToken = service.getRequestToken
    println( "Go here to get the pin https://twitter.com/oauth/authorize?oauth_token=" + requestToken.getToken )
    System.out.println("And paste the pin here")
    System.out.print(">>")

    val verifier = new Verifier(new Scanner(System.in).nextLine())
    val accessToken = service.getAccessToken( requestToken, verifier )
    println( "Access token: " + accessToken.getToken + " secret: " + accessToken.getSecret )

  }

  private def parseRequest( request:OAuthRequest ) =
    try{
      val JField( _, JInt( id ) ) = parse( request.send.getBody ) \ "id"
      Some( id.longValue )
    }
    catch{
      case exp =>
        exp.printStackTrace()
        None
    }


  def update( message:String ) = {
    val request = new OAuthRequest( Verb.POST, "http://api.twitter.com/1/statuses/update.json" )
    request.addBodyParameter( "status", message )
    service.signRequest( Twitter.accessToken, request )
    parseRequest( request )

  }

  def destroy( messageId:Long ) = {
    val request = new OAuthRequest( Verb.POST, "http://api.twitter.com/1/statuses/destroy/"+messageId.toString+".json" )
    service.signRequest( Twitter.accessToken, request )
    parseRequest( request )
  }

}
