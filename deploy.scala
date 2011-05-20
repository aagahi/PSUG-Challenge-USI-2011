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

  val JRE = "jre-6u23-linux-i586.bin"
  val EOL = getProperty("line.separator")
  val PATH = getProperty("path.separator")

  val cwd = new File(System.getProperty("user.dir"))
  val DEFAULT_PORT = "8082"

  def system(cmd: String*): Process = {
    new ProcessBuilder(cmd: _*).redirectErrorStream(true).start
  }

  def system(cmd: Array[String], env: Map[String, String]): Process = {
    val pb = new ProcessBuilder(cmd: _*).redirectErrorStream(true)
    for ((k, v) <- env) pb.environment.put(k, v)
    pb.start
  }

  def system(cmd: Array[String], wd: File): Process =
    new ProcessBuilder(cmd: _*).redirectErrorStream(true).directory(wd).start

  def assertInRootDirectory(f: => Unit): Unit = {
    try {
      (loadFile(new File(cwd, "pom.xml")) \ "artifactId").text == "challenge-usi"
    } catch {
      case e => f
    }
  }

  def collectOutput(proc: Process): Unit =
    new Thread {
      override def run() = for (c <- fromInputStream(proc.getInputStream)) print(c)
    }.start

  def expectOutput(proc: Process, re: String, timeout: Int): Thread = {
    val th = new Thread {
      override def run: Unit = {
        val sb = new StringBuilder
        for (c <- fromInputStream(proc.getInputStream)) {
          sb.append(c)
          if (sb.toString.matches(re))
            return ()
        }
      }
    }
    th.start
    th.join(timeout)
    th.interrupt
    th
  }

  def reap(proc: Process): Unit =
    Runtime.getRuntime().addShutdownHook(new Thread {
      override def run() = proc.destroy
    })

  def copyFile(from: File, to: File): Boolean = {
    try {
      val out = new FileOutputStream(to)
      val in = new FileInputStream(from)
      val buf = new Array[Byte](1024)
      var ln: Int = 0
      while (ln != -1) {
        ln = in.read(buf, 0, 1024)
        out.write(buf, 0, ln)
      }
      true
    } catch {
      case e => false
    }
  }

  def mvnExecutable = if (getProperty("os.name").toLowerCase.startsWith("windows")) "mvn.bat" else "mvn"

  def run(cmds: String*)(errMsg: String, okMsg: String): Unit = {
    val proc = system(cmds: _*)
    collectOutput(proc)
    reap(proc)
    if (proc.waitFor != 0) {
      println(errMsg)
      exit(1)
    } else
      println(okMsg)
  }

  def main(args: Array[String]) = {

    if (args.length != 1) {
      println("Remote host needed")
      exit(1)
    }

    val host::Nil = args.toList

    val deploymentDirectory = "/home/user/challenge-usi"
    val user = "user"

    assertInRootDirectory {
      println("Not in toplevel directory for project challenge-usi, giving up")
      exit(1)
    }

    val dist = new File("target/challenge-usi-dist.zip")
    if (!dist.exists) {
      println("Jar file for project challenge-usi does not exist. Have you run 'mvn package'?")
      exit(1)
    } else
      println("Distribution archive exists OK")
    val script = new File("script/challenge-usi-server")
    if (!script.exists) {
      println("Script file for project challenge-usi does not exist!")
      exit(1)
    } else
      println("Script exists OK")

    println( "Did you check?\nakka.conf: hostname should be vfabric1\nconfiguration.properties: services.host should be vfabric1\nlogback.xml root level should be warn" )


    // assume public key is set up and ssh is in path
    run("ssh", user + "@" + host, "mkdir -p " + deploymentDirectory )("fail to create remote directory " + host + ":" + deploymentDirectory + ". Check credentials, network and other usual suspects for SSH related stuff",
      "Deployment directory " + host + ":" + deploymentDirectory + " OK")

    run("scp", dist.getPath, user + "@" + host + ":" + deploymentDirectory + "/challenge-usi-dist.zip")("Cannot remote copy distribution archive to " + host + ". Check credentials, network and other usual suspects for SSH related stuff", "remote copy archive " + dist + " to " + host + ":" + deploymentDirectory + " OK")


    run("scp", script.getPath, user + "@" + host + ":" + deploymentDirectory )("Cannot remote copy script to " + host + ". Check credentials, network and other usual suspects for SSH related stuff", "Remote copy archive " + dist + " to " + host + ":" + deploymentDirectory + " OK")

    run("ssh", user + "@" + host, "cd " + deploymentDirectory + "; jar xf challenge-usi-dist.zip")( "Unjar failure", "Success" )

    run("rsync", "-az", "--delete", "web",  user + "@" + host + ":" + deploymentDirectory)("Upload web folder failure","Upload web folder success")

    run("ssh", user + "@" + host, "rsync -az --delete challenge-usi user@vfabric2:/home/user/")("Deploy on vfabric2 failure","Deploy on vfabric2 success")
    run("ssh", user + "@" + host, "rsync -az --delete challenge-usi user@vfabric3:/home/user/")("Deploy on vfabric3 failure","Deploy on vfabric3 success")
    run("ssh", user + "@" + host, "rsync -az --delete challenge-usi user@vfabric4:/home/user/")("Deploy on vfabric4 failure","Deploy on vfabric4 success")

    run("ssh", user + "@" + host, "rsync -az --delete challenge-usi user@usi1:/home/user/")("Deploy on usi1 failure","Deploy on usi1 success")
  }
}

deploy.main(args)
