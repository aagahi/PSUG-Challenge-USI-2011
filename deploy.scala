/**
 * Script de deploiement du serveur USI 2011.
 *  - mvn et ssh sont dans le path
 *  - attend un nom de host, un répertoire et un port pour le lancement du serveur
 *  - recupère l'ensemble des dépendances, y compris un JRE, les empaquette puis
 *    les copies dans le répertoire de destination
 *  - lance le serveur sur le port indiqué sur le host choisi
 */

import java.io._
import scala.io.Source._
import scala.xml.XML._
import System._

object deploy { 

  val JRE		= "jre-6u23-linux-i586.bin"
  val EOL		= getProperty("line.separator")
  val PATH		= getProperty("path.separator")

  val cwd		= new File(System.getProperty("user.dir"))
  val DEFAULT_PORT	= "8082"

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

  def assertInRootDirectory (f : => Unit): Unit = { 
    try { 
      (loadFile(new File(cwd,"pom.xml")) \ "artifactId").text == "challenge-usi"
    } catch { 
      case e => f
    }
  }

  def collectOutput(proc : Process) : Unit =   
    new Thread {  override def run() = for(c <- fromInputStream(proc.getInputStream)) print(c) }.start

  def expectOutput(proc : Process, re : String, timeout : Int) : Thread =  {
    val th = new Thread { 
      override def run : Unit = { 
	val sb = new StringBuilder
	for(c <- fromInputStream(proc.getInputStream)) { 
	  sb.append(c)
	  if(sb.toString.matches(re))
	    return ()
	}
      }
    }
    th.start
    th.join(timeout)
    th.interrupt
    th
  }
  
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

  def run(cmds: String*)(errMsg : String, okMsg : String) : Unit = { 
    val proc = system(cmds:_*)
    collectOutput(proc)
    reap(proc)
    if(proc.waitFor != 0) { 
      println(errMsg)
      exit(1)
    } else
      println(okMsg)
  }

  def main(args: Array[String]) = { 
    if(args.length != 4) { 
      println("not enough arguments, need port, remote host, remote deployment directory and user")
      exit(1)
    }
      
    val (port :: host :: deploymentDirectory :: user :: Nil) = args.toList

    assertInRootDirectory { 
      println("Not in toplevel directory for project challenge-usi, giving up")
      exit(1)
    }
    
    val dist = new File("target/challenge-usi-dist.zip")
    if(!dist.exists) { 
      println("Jar file for project challenge-usi does not exist. Have you run 'mvn package'?")
      exit(1)
    } else 
      println("distribution archive exists OK")

    // assume public key is set up and ssh is in path
    run("ssh","-C","-o","PasswordAuthentication no",user + "@" +host,"mkdir -p " + deploymentDirectory +"")("fail to create remote directory "+host+":"+ deploymentDirectory +". Check credentials, network and other usual suspects for SSH related stuff", 
								      "deployment directory "+ host +":"+ deploymentDirectory+" OK")

    run("scp",dist.getPath,user + "@" +host + ":" + deploymentDirectory +"/challenge-usi-dist.zip")("Cannot remote copy distribution archive to "+host+". Check credentials, network and other usual suspects for SSH related stuff","remote copy archive "+ dist + " to "+ host +":"+ deploymentDirectory+" OK")

    val java = system("ssh","-batch",user + "@" +host,"cd " + deploymentDirectory + "; jar xf challenge-usi-dist.zip; nohup java -jar challenge-usi.jar "+port+" &")
    val thread = expectOutput(java,".*Starting PSUG USI2011 Challenge on port "+port+".*",60000)
    if(thread.isInterrupted)
      println("failed to launch web application on port "+port)
    else
      println("launch web application at "+host+":"+port+" in "+ deploymentDirectory+" OK")
    java.destroy
  }
}

deploy.main(args)
