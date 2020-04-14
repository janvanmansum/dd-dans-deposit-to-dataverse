/**
 * Copyright (C) 2020 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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
