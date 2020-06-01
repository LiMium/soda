package soda.grinder

import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.ServerSocket
import java.net.Socket
import java.io.FileInputStream
import java.io.DataInputStream
import java.io.File
import java.io.FileOutputStream
import java.io.DataOutputStream
import java.io.BufferedWriter
import java.net.URL
import soda.Parser
import soda.Renderer
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.ByteArrayOutputStream
import soda.utils.Util
import soda.layout.config

object GrinderApp {
  val ss = new ServerSocket(0)

  private def getPortFile() = {
    val userHome = System.getProperty("user.home")
    val appHome = new File(userHome, ".gngr")
    val profileHome = new File(appHome, "default")
    new File(profileHome, "port.dat")
  }

  private def setGngrPort(portNum: Int) = {
    val portFile = getPortFile
    if (portFile.exists) {
      throw new RuntimeException("`gngr` is probably running as port file exists")
    }

    val out = new FileOutputStream(portFile)
    val dout = new DataOutputStream(out)
    dout.writeInt(portNum)
    dout.close()
  }

  private def startGngrServer() = {
    val gss = new ServerSocket(0)
    val lp = gss.getLocalPort
    setGngrPort(lp)
    println("Saved gngr port: " + lp)
    val s = gss.accept()
    val in = s.getInputStream
    val reader = new InputStreamReader(in)
    val br = new BufferedReader(reader)
    var line:String = null
    var command = false
    do {
      line = br.readLine
      println(line)
      if (line != null && line.startsWith("GRINDER")) {
        command = true
      }
    } while (line != null && !command)
    val out = s.getOutputStream
    val dos = new DataOutputStream(out)
    dos.writeInt(ss.getLocalPort)
    dos.close
  }

  private def startServer() = {
    println("Accepting")
    var done = false
    while (!done) {
      val s = ss.accept()
      s.setSoTimeout(10000)
      s.setTcpNoDelay(true)

      val in = s.getInputStream
      val reader = new InputStreamReader(in)
      val br = new BufferedReader(reader)
      val commandLine = br.readLine()
      if (commandLine != null) {
        val blankIdx = commandLine.indexOf(' ');
        val command = if (blankIdx == -1) commandLine else commandLine.substring(0, blankIdx).trim();
        // System.out.println("Command: " + command);
        if ("TO".equals(command)) {
          if (blankIdx != -1) {
            val path = commandLine.substring(blankIdx + 1).trim();
            handleTo(s, br, path);
          }
        } else if ("SET_SIZE".equals(command)) {
          if (blankIdx != -1) {
            val params = commandLine.substring(blankIdx + 1).trim();
            handleResize(s, br, params);
          }
        } else if ("SCREENSHOT".equals(command)) {
          handleScreenShot(s, br);
        } else if ("CLOSE".equals(command)) {
          // frame.closeWindow();
          done = true;
        } else if ("QUIT".equals(command)) {
          // frame.closeWindow();
          // PlatformInit.shutdown();
          val portFile = getPortFile()
          portFile.deleteOnExit()
          done = true;
        }

      }
    }
  }

  private var outImg : BufferedImage = null

  private def handleTo(s: Socket, br: BufferedReader, path: String) = {
    println("To " + path)
    val url = new URL("http://" + path)
    outImg = Renderer.render(url, "")

    markDoneAndWaitForAck(s, br)
  }

  private def handleScreenShot(s: Socket, br: BufferedReader) = {
    val bos = new ByteArrayOutputStream();
    ImageIO.write(outImg, "PNG", bos)
    val os = s.getOutputStream
    val dos = new DataOutputStream(os)
    dos.writeInt(bos.size)
    dos.flush()
    bos.writeTo(os)
    os.flush()

    // Wait for ack
    br.readLine()
  }

  private var width = 800
  private var height  = 800

  private def handleResize(s: Socket, br: BufferedReader, params: String) = {
    val fields = params.split(" ")
    width = fields(0).toInt
    height = fields(1).toInt

    println(s"  setting width: $width, height: $height")

    markDoneAndWaitForAck(s, br)
  }

  private def markDoneAndWaitForAck(s: Socket, br: BufferedReader) = {
    val os = new DataOutputStream(s.getOutputStream)
    os.writeInt(0)
    os.flush()

    // Wait for Ack
    br.readLine
  }

  def main(args: Array[String]) = {
    if (config.affectsRender) {
      Util.warnln("Paint debug level is too high or showBoxes is true")
    } else {
      startGngrServer()
      startServer()
    }
  }
}
