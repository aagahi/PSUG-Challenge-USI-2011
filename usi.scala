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
      (loadFile(new File(cwd,"pom.xml")), "artifactId").text == "challenge-usi"
    } catch { 
      case e => false
    }
  }

  def collectOutput(proc : Process) : Unit =   
    new Thread {  override def run() = for(c <- fromInputStream(proc.getInputStream)) print(c) }.start

  def reap(proc : Process) : Unit = 
    Runtime.getRuntime().addShutdownHook(new Thread { override def run() = proc.destroy })

  def mvnExecutable = if(getProperty("os.name").toLowerCase.startsWith("windows")) "mvn.bat" else "mvn"

  def main(args: Array[String]) = { 
    val port = if(args.length > 0) args(0) else DEFAULT_PORT

    if(!assertInRootDirectory) { 
      println("Not in toplevel directory for project challenge-usi, giving up")
      exit(1)
    }
    
    val pb = system(mvnExecutable,"-q","dependency:build-classpath","-Dmdep.outputFile=" + cpFile)

    if(pb.waitFor != 0) { 
      println("Failed to generate classpath.txt. Is maven in your PATH?")
      println("If so, check that you configured your #/.m2/setting.xml to activate 'with-repo' profile")
      println("See http://maven.apache.org/settings.html#Active_Profiles")
      exit(1)
    } 

    val classpath = "target/classes" + PATH + fromFile(new File(cpFile)).mkString

    val java = system(Array("java","org.psug.usi.Main",port), Map("CLASSPATH" -> classpath))
    
    collectOutput(java)
    reap(java)
    java.waitFor
  }
}


usi.main(args)
