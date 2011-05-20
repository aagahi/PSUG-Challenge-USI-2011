/**
 * Script de lancement du serveur USI 2011 en mode developpement. 
 *  - mvn est dans le PATH
 *  - le repertoire courant est la racine du projet challenge-usi
 * Usage: scala usi.scala [port]
 */

import java.io._
import scala.io.Source._
import scala.xml.XML._
import System._

object usi { 

  val EOL = getProperty("line.separator")
  val PATH = getProperty("path.separator")
  val DEFAULT_PORT = "8082"

  val cpFile = "target/classpath.txt"

  def system (cmd: String*): Process = {
    new ProcessBuilder (cmd:_*).redirectErrorStream(true).start
  }

  def system (cmd: Array[String], env : Map[String,String]): Process = {
    val pb = new ProcessBuilder (cmd:_*).redirectErrorStream(true)
    for((k,v) <- env) pb.environment.put(k,v)
    pb.start
  }

  def assertInRootDirectory : Boolean = { 
    val cwd = new File(System.getProperty("user.dir"))
    try { 
      (loadFile(new File(cwd,"pom.xml")) \ "artifactId").text == "challenge-usi"
    } catch { 
      case e => false
    }
  }

  def collectOutput(proc : Process, to : PrintStream) : Unit =   
    new Thread {  override def run() = for(c <- fromInputStream(proc.getInputStream)) to.print(c) }.start

  def reap(proc : Process) : Unit = 
    Runtime.getRuntime().addShutdownHook(new Thread { override def run() = proc.destroy })

  def mvnExecutable = if(getProperty("os.name").toLowerCase.startsWith("windows")) "mvn.bat" else "mvn"

  def main(args: Array[String]) = { 

    if(!assertInRootDirectory) { 
      println("Not in toplevel directory for project challenge-usi, giving up")
      exit(1)
    }
    
    val pb = system(mvnExecutable,"-q","dependency:build-classpath","-Dmdep.outputFile=" + cpFile)
    collectOutput(pb,out)
    reap(pb)

    if(pb.waitFor != 0) { 
      println("Failed to generate classpath.txt. Is maven in your PATH?")
      println("If so, check that you configured your #/.m2/setting.xml to activate 'with-repo' profile")
      println("See http://maven.apache.org/settings.html#Active_Profiles")
      exit(1)
    } 

    val classpath = "target/classes" + PATH + fromFile(new File(cpFile)).mkString

    println("generated classpath ")
    val java = system(Array("java","org.psug.usi.Main"), Map("CLASSPATH" -> classpath))
    
    collectOutput(java,out)
    reap(java)

    println("Type <ENTER> to exit")
    while(System.in.read() == -1)
      wait(500)
    java.destroy
    java.waitFor
  }
}


usi.main(args)
