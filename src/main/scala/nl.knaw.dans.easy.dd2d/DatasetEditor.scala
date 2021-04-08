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

import better.files.File
import nl.knaw.dans.lib.dataverse.model.file.FileMeta
import nl.knaw.dans.lib.dataverse.model.file.prestaged.DataFile
import nl.knaw.dans.lib.dataverse.{ DatasetApi, DataverseInstance }
import nl.knaw.dans.lib.error.TraversableTryExtensions
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.Try

/**
 * Object that edits a dataset, a new draft.
 */
abstract class DatasetEditor(deposit: Deposit, instance: DataverseInstance) extends DebugEnhancedLogging {
  type PersistendId = String
  protected val dataset: DatasetApi = instance.dataset(deposit.dataversePid)

  /**
   * Performs the task.
   *
   * @return the persistentId of the dataset created or modified
   */
  def performEdit(): Try[PersistendId]

  /**
   * Adds files to the dataset. If the file is pre-staged it is registered instead of uploaded.
   *
   * @param persistentId            the dataset dOI
   * @param files                   list of FileInfo objects
   * @param checksumToPrestagedFile map of checksum to pre-staged DataFile object
   * @return a Map from file database ID to FileInfo of the added file
   */
  protected def addFiles(persistentId: String, files: List[FileInfo], checksumToPrestagedFile: Map[String, DataFile] = Map.empty): Try[Map[Int, FileInfo]] = {
    trace(persistentId, files)
    files
      .map(f => {
        debug(s"Adding file, SHA-1 = ${ f.checksum }, directoryLabel = ${ f.metadata.directoryLabel }, label = ${ f.metadata.label }")
        for {
          id <- addFile(persistentId, f, checksumToPrestagedFile)
          _ <- instance.dataset(persistentId).awaitUnlock()
        } yield (id -> f)
      }).collectResults.map(_.toMap)
  }

  private def addFile(doi: String, fileInfo: FileInfo, checksumToPrestagedFile: Map[String, DataFile]): Try[Int] = {
    val result =
      for {
        r <-
          if (checksumToPrestagedFile.contains(fileInfo.checksum)) {
            debug(s"Adding prestaged file: $fileInfo")
            instance.dataset(doi).registerPrestagedFile(checksumToPrestagedFile(fileInfo.checksum))
          }
          else {
            debug(s"Uploading file: $fileInfo")
            instance.dataset(doi).addFile(Option(fileInfo.file))
          }
        files <- r.data
        id = files.files.headOption.flatMap(_.dataFile.map(_.id))
      } yield id
    result.map(_.getOrElse(throw new IllegalStateException("Could not get DataFile ID from response")))
  }

  protected def updateFileMetadata(databaseIdToFileInfo: Map[Int, FileMeta]): Try[Unit] = {
    databaseIdToFileInfo.map { case (id, fileMeta) => instance.file(id).updateMetadata(fileMeta) }.collectResults.map(_ => ())
  }

  protected def deleteFiles(databaseIds: List[DatabaseId]): Try[Unit] = {
    databaseIds.map(id => {
      debug(s"Deleting file, databaseId = $id")
      instance.sword().deleteFile(id).map(_ => dataset.awaitUnlock())
    }).collectResults.map(_ => ())
  }

  protected def replaceFiles(databaseIdToNewFile: Map[Int, File]): Try[Map[Int, FileMeta]] = {
    trace(databaseIdToNewFile)
    databaseIdToNewFile.map {
      case (id, file) =>
        val fileApi = instance.file(id)

        for {
          r <- fileApi.replace(Option(file))
          d <- r.data
          _ <- dataset.awaitUnlock()
        } yield (d.files.head.dataFile.get.id, d.files.head)
    }.collectResults.map(_.toMap)
  }

}
