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
package nl.knaw.dans.easy.dd2d

import nl.knaw.dans.easy.dd2d.migrationinfo.MigrationInfo
import nl.knaw.dans.lib.dataverse.model.dataset.{ Dataset, DatasetCreationResult }
import nl.knaw.dans.lib.dataverse.model.file.prestaged.DataFile
import nl.knaw.dans.lib.dataverse.{ DataverseInstance, DataverseResponse }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Success, Try }

class DatasetCreator(deposit: Deposit, dataverseDataset: Dataset, instance: DataverseInstance, migrationInfo: Option[MigrationInfo]) extends DatasetEditor(deposit, instance) with DebugEnhancedLogging {
  trace(deposit)

  override def performEdit(): Try[PersistendId] = {
    for {
      // autoPublish is false, because it seems there is a bug with it in Dataverse (most of the time?)
      response <- if (deposit.doi.nonEmpty)
                    instance
                      .dataverse("root")
                      .importDataset(dataverseDataset, Some(s"doi:${ deposit.doi }"), autoPublish = false)
                  else instance.dataverse("root").createDataset(dataverseDataset)
      persistentId <- getPersistentId(response)
      fileInfos <- deposit.getPathToFileInfo
      prestagedFiles <-  migrationInfo.map(_.getPrestagedDataFilesFor(deposit.doi)).getOrElse(Success((Map.empty[String, DataFile])))
      _ = debug(s"Pre-staged files: $prestagedFiles")
      databaseIdsToFileInfo <- addFiles(persistentId, fileInfos.values.toList, prestagedFiles)
      _ <- updateFileMetadata(databaseIdsToFileInfo)
    } yield persistentId
  }

  private def getPersistentId(response: DataverseResponse[DatasetCreationResult]): Try[String] = {
    response.data.map(_.persistentId)
  }
}
