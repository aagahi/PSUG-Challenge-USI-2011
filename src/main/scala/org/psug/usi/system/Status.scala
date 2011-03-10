package org.psug.usi.system

/**
 * Created by IntelliJ IDEA.
 * User: abailly
 * Date: 07/03/11
 * Time: 22:28
 * To change this template use File | Settings | File Templates.
 */


/**
 *
 * @param nodeType type of node for the current system. Values are enumerated in NodeTypes object.
 * @param port port server is listening to for incoming requests.
 */
case class Status(nodeType : String, port: Int)

object NodeTypes {
  val web = "Web"
  val service = "Service"
}