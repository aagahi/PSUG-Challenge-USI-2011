package org.psug.usi.system

/**
 * A trait for exiting the system.
 * This is abstracted away to ease testing.
 */
trait Exit { 
  def exit : Unit
}

object systemExit extends Exit { 
  override def exit = new Thread { override def run = println("Exiting..."); Thread.sleep(2000); System.exit(0) }.start 
}
