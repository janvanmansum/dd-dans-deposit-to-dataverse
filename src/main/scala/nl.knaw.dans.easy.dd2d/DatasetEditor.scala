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

import nl.knaw.dans.easy.dd2d.migrationinfo.BasicFileMeta
import nl.knaw.dans.lib.dataverse.DataverseInstance
import nl.knaw.dans.lib.dataverse.model.file.FileMeta
import nl.knaw.dans.lib.dataverse.model.file.prestaged.DataFile
import nl.knaw.dans.lib.error.TraversableTryExtensions
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.Try

/**
 * Object that edits a dataset, a new draft.
 */
abstract class DatasetEditor(instance: DataverseInstance) extends DebugEnhancedLogging {
  type PersistendId = String

  /**
   * Performs the task.
   *
   * @return the persistentId of the dataset created or modified
   */
  def performEdit(): Try[PersistendId]

  protected def addFiles(persistentId: String, files: List[FileInfo], prestagedFiles: Set[BasicFileMeta] = Set.empty): Try[Map[Int, FileInfo]] = {
    trace(persistentId, files)
    files
      .map(f => {
        debug(s"Adding file, directoryLabel = ${ f.metadata.directoryLabel }, label = ${ f.metadata.label }")
        for {
          id <- addFile2(persistentId, f, prestagedFiles)
          _ <- instance.dataset(persistentId).awaitUnlock()
        } yield (id -> f)
      }).collectResults.map(_.toMap)
  }

  private def addFile2(doi: String, fileInfo: FileInfo, prestagedFiles: Set[BasicFileMeta]): Try[Int] = {
    val result = for {
      r <- getPrestagedFileFor(fileInfo, prestagedFiles).map { dataFile =>
        debug(s"Adding prestaged file: $fileInfo")
        instance.dataset(doi).addPrestagedFile(dataFile)
      }.getOrElse {
        debug(s"Uploading file: $fileInfo")
        instance.dataset(doi).addFile(Option(fileInfo.file))
      }
      files <- r.data
      id = files.files.headOption.flatMap(_.dataFile.map(_.id))
      _ <- instance.dataset(doi).awaitUnlock()
    } yield id
    result.map(_.getOrElse(throw new IllegalStateException("Could not get DataFile ID from response")))
  }

  private def getPrestagedFileFor(fileInfo: FileInfo, basicFileMetas: Set[BasicFileMeta]): Option[DataFile] = {
    val matchingChecksums = basicFileMetas.filter(_.dataFile.checksum.`@value` == fileInfo.checksum)
    if (matchingChecksums.size == 1) Option(matchingChecksums.head.dataFile)
    else if (matchingChecksums.isEmpty) Option.empty
         else {
           val matchingPaths = basicFileMetas.filter(bfm => bfm.label == fileInfo.metadata.label.get && bfm.directoryLabel == fileInfo.metadata.directoryLabel)
           if (matchingPaths.size == 1) Option(matchingPaths.head.dataFile)
           else if (matchingPaths.isEmpty) Option.empty
                else throw new IllegalArgumentException("Found multiple basic file metas with the same path in a single dataset version")
         }
  }

  protected def updateFileMetadata(databaseIdToFileInfo: Map[Int, FileMeta]): Try[Unit] = {
    databaseIdToFileInfo.map { case (id, fileMeta) => instance.file(id).updateMetadata(fileMeta) }.collectResults.map(_ => ())
  }
}
