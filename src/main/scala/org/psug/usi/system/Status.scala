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
 * @param webPort port web server is listening to for incoming requests. Note that there is always a web server listening,
 * even for service type nodes, as it is the main control interface.
 * @param servicesPort optional value for service type nodes where the remote services lay.
 */
case class Status(nodeType : String, webPort: Int, servicesPort: Option[Int] = None)

object NodeTypes {
  val web = "Web"
  val service = "Service"
}