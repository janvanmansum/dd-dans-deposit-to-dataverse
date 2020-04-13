package nl.knaw.dans.easy.s2d.queue
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.Try

case class TriggerTask() extends Task with DebugEnhancedLogging {
  var triggered = false

  override def run(): Try[Unit] = Try {
    trace(())
    triggered = true
  }
}
