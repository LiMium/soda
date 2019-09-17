package soda

import java.net.URL
import javax.imageio.ImageIO
import java.io.File

object Main {
  def main(args: Array[String]):Unit = {
    val url = new URL("http://127.0.0.1:9000/grinder/nightly-unstable/xhtml1/border-top-width-095.xht")

    // println(dom.walkAllElements().mkString(","))
    val outImg = Renderer.render(url, "")

    ImageIO.write(outImg, "PNG", new File("urlOut.png"))
  }
}
