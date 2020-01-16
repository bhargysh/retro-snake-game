package snake

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import org.scalajs.dom.Window

@js.native
@JSImport("jsdom", JSImport.Namespace)
class JSDom extends js.Object {

  def window: Window = js.native

}
