package soda

import java.net.URL
import javax.imageio.ImageIO
import java.io.File

object Main {
  def main(args: Array[String]):Unit = {
    // val url = new URL("http://127.0.0.1:9000/grinder/nightly-unstable/xhtml1/width-percentage-001.xht")
    // val url = new URL("http://127.0.0.1:9000/grinder/nightly-unstable/xhtml1/max-width-110.xht")
    // val url = new URL("http://localhost:8000/nightly-unstable/xhtml1/cascade-precedence-001.xht")
    // val url = new URL("http://localhost:8000/nightly-unstable/xhtml1/reference/ref-if-there-is-no-red.xht")
    // val url = new URL("http://localhost:8000//nightly-unstable/xhtml1/absolute-non-replaced-width-003.xht")
    // val url = new URL("http://127.0.0.1:9000/grinder/nightly-unstable/xhtml1/reference/ref-filled-black-96px-square.xht")
    // val url = new URL("http://localhost:8000//nightly-unstable/xhtml1/replaced-elements-min-width-40.xht")
    // val url = new URL("http://127.0.0.1:9000/grinder/nightly-unstable/xhtml1/reference/pass_if_square_96px_black.xht")
    // val url = new URL("http://127.0.0.1:9000/grinder/nightly-unstable/xhtml1/border-width-014.xht")
    val url = new URL("http://127.0.0.1:9000/grinder/nightly-unstable/xhtml1/floats-142.xht")
    // val url = new URL("http://127.0.0.1:9000/grinder/nightly-unstable/xhtml1/reference/ref-filled-green-100px-square.xht")

    // println(dom.walkAllElements().mkString(","))
    val outImg = Renderer.render(url, "")

    ImageIO.write(outImg, "PNG", new File("urlOut.png"))
  }
}
