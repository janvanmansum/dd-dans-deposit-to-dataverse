package nl.knaw.dans.easy.s2d

import nl.knaw.dans.easy.s2d.queue.Task
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.Try

case class DepositIngestTask(deposit: Deposit, dataverse: Dataverse) extends Task with DebugEnhancedLogging {
  override def run(): Try[Unit] = Try {
    trace(())
    debug(s"Ingesting $deposit into Dataverse")

    // Read title from metadata


    // 


  }
}
