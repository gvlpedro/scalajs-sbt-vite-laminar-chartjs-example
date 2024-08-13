package testvite

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

object Main {
  def main(args: Array[String]): Unit = {
    renderOnDomContentLoaded(dom.document.querySelector("#app"), Chat.render())
  }
}
