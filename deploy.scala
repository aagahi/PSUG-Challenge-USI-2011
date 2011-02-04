/**
 * Script de deploiement du serveur USI 2011.
 *  - mvn et ssh sont dans le path
 *  - attend un nom de host, un répertoire et un port pour le lancement du serveur
 *  - recupère l'ensemble des dépendances, y compris un JRE, les empaquette puis
 *    les copies dans le répertoire de destination
 *  - lance le serveur sur le port indiqué sur le host choisi
 */

import java.io._
import scala.xml.pull._
import scala.io.Source._
import Stream._
import scala.xml.XML._
import System._

object deploy { 

  val EOL = getProperty("line.separator")
  val PATH = getProperty("path.separator")
  val cpFile = "target/classpath.txt"
  val cwd = new File(System.getProperty("user.dir"))
  val DEFAULT_PORT = "8082"

  def system (cmd: String*): Process = {
    new ProcessBuilder (cmd:_*).redirectErrorStream(true).start
  }

  def system (cmd: Array[String], env : Map[String,String]): Process = {
    val pb = new ProcessBuilder (cmd:_*).redirectErrorStream(true)
    for((k,v) <- env) pb.environment.put(k,v)
    pb.start
  }

  def system (cmd: Array[String], wd : File): Process = 
    new ProcessBuilder (cmd:_*).redirectErrorStream(true).directory(wd).start

  def assertInRootDirectory : Boolean = { 
    try { 
      (loadFile(new File(cwd,"pom.xml")) \ "artifactId").text == "challenge-usi"
    } catch { 
      case e => false
    }
  }

  def collectOutput(proc : Process) : Unit =   
    new Thread {  override def run() = for(c <- fromInputStream(proc.getInputStream)) print(c) }.start

  def reap(proc : Process) : Unit = 
    Runtime.getRuntime().addShutdownHook(new Thread { override def run() = proc.destroy })

  def copyFile(from : File, to : File) : Boolean = { 
    try { 
      val out = new FileOutputStream(to)
      val in = new FileInputStream(from)
      val buf = new Array[Byte](1024)
      var ln : Int = 0
      while(ln != -1) { 
	ln = in.read(buf,0,1024)
	out.write(buf,0,ln)
      }
      true
    }  catch { 
      case e => false
    }
  }

  def mvnExecutable = if(getProperty("os.name").toLowerCase.startsWith("windows")) "mvn.bat" else "mvn"

  def main(args: Array[String]) = { 
    val port = if(args.length > 0) args(0) else DEFAULT_PORT

    if(!assertInRootDirectory) { 
      println("Not in toplevel directory for project challenge-usi, giving up")
      exit(1)
    }
    
    val deployDir = new File("target/deployment") 
    val libDir = new File(deployDir,"lib") 

    if(!libDir.exists && !libDir.mkdirs) { 
      println("Cannot create directory 'target/deployment/lib', check permissions")
      exit(1)
    }

    val pb = system(mvnExecutable,"-q","dependency:copy-dependencies","-DincludeScope=runtime","-DoutputDirectory=" + libDir.getAbsolutePath)

    if(pb.waitFor != 0) { 
      println("Failed to copy needed dependencies to deployment directory. Is maven in your PATH?")
      exit(1)
    } 

    val version = (loadFile(new File(cwd,"pom.xml")) \ "version").text

    val jar = new File("target/challenge-usi-"+version+".jar")

    if(!jar.exists) { 
      println("Jar file for project challenge-usi does not exist. Have you run 'mvn package'")
      exit(1)
    }

    copyFile(jar,new File(deployDir,jar.getName))

    val java = system(Array("java","-jar",jar.getName, port),jar.getParentFile)
    
    collectOutput(java)
    reap(java)
    java.waitFor
  }
}

deploy.main(args)
