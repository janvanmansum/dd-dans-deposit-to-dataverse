package nl.knaw.dans.easy.s2d

import java.util.concurrent.Executors

import better.files.{ File, FileMonitor }
import nl.knaw.dans.easy.s2d.queue.ActiveTaskQueue
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.concurrent.ExecutionContext

class InboxMonitor(inbox: File, dataverse: Dataverse) extends DebugEnhancedLogging {
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  private val ingestTasks = new ActiveTaskQueue()
  private val watcher = new FileMonitor(inbox, maxDepth = 1) {
    override def onCreate(d: File, count: Int): Unit = {
      if (d.isDirectory) {
        ingestTasks.add(DepositIngestTask(Deposit(d), dataverse))
      }
    }
  }

  def start(): Unit = {
    trace(())
    logger.info("Queueing existing directories...")
    inbox.list(_.isDirectory).filterNot(_ == inbox).foreach {
      d => {
        debug(s"Adding $d")
        ingestTasks.add(DepositIngestTask(Deposit(d), dataverse))
      }
    }

    logger.info("Starting inbox watcher...")
    ingestTasks.start()
    watcher.start()
  }

  def stop(): Unit = {
    trace(())
    watcher.stop()
    ingestTasks.stop()
  }
}
