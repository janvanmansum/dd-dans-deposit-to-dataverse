package nl.knaw.dans.easy.s2d.queue

import java.util.concurrent.{ Executors, LinkedBlockingDeque }

import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.concurrent.ExecutionContext

/**
 * A queue that processes Tasks on a dedicated thread.
 *
 * @param capacity the maximum capacity of the queue
 */
class ActiveTaskQueue(capacity: Int = 200) extends DebugEnhancedLogging {
  private val executionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  private val tasks = new LinkedBlockingDeque[Option[Task]](capacity)

  /**
   * Adds a new task to the queue.
   *
   * @param t the task to add
   */
  def add(t: Task): Unit = {
    tasks.put(Some(t))
  }

  /**
   * Starts the queue's processing thread.
   */
  def start(): Unit = {
    executionContext.execute(runnable = () => {
      logger.info("Processing thread ready for running tasks")
      while (runTask(tasks.take())) {}
      logger.info("Finished processing tasks.")
    })
  }

  private def runTask(t: Option[Task]): Boolean = {
    t.map(_.run().recover {
      case e: Throwable => logger.warn(s"Task $t failed", e);
    }).isDefined
  }

  /**
   * Cancels pending tasks and lets the currently running task finish. Then lets the
   * processing thread terminate.
   */
  def stop(): Unit = {
    tasks.clear()
    tasks.put(Option.empty[Task])
  }
}
