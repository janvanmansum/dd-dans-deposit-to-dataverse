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

import nl.knaw.dans.lib.dataverse.DataverseInstance
import nl.knaw.dans.lib.dataverse.model.dataset.MetadataBlocks
import nl.knaw.dans.lib.dataverse.model.file.FileMeta
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import java.nio.file.{ Path, Paths }
import scala.util.{ Failure, Success, Try }

class DatasetUpdater(deposit: Deposit, metadataBlocks: MetadataBlocks, instance: DataverseInstance) extends DatasetEditor(deposit, instance) with DebugEnhancedLogging {
  trace(deposit)

  override def performEdit(): Try[PersistendId] = {
    for {
      _ <- dataset.awaitUnlock()
      _ <- dataset.updateMetadata(metadataBlocks)

      _ <- dataset.awaitUnlock()
      pathToFileInfo <- deposit.getPathToFileInfo
      pathToFileMetaInLatestVersion <- getFilesInLatestVersion
      _ <- validateFileMetas(pathToFileMetaInLatestVersion.values.toList)

      fileReplacements <- getFilesToReplace(pathToFileInfo, pathToFileMetaInLatestVersion)
      _ <- replaceFiles(fileReplacements.mapValues(fileInfo => fileInfo.file))

      oldToNewPathMovedFiles <- getOldToNewPathOfFilesToMove(pathToFileMetaInLatestVersion, pathToFileInfo)
      fileMovements = oldToNewPathMovedFiles.map { case (old, newPath) => (pathToFileMetaInLatestVersion(old).dataFile.get.id, pathToFileInfo(newPath)) }
      // Movement will be realized by updating label and directoryLabel attributes of the file

      pathsToDelete = pathToFileMetaInLatestVersion.keySet diff pathToFileInfo.keySet diff oldToNewPathMovedFiles.keySet
      fileDeletions <- getFileDeletions(pathsToDelete, pathToFileMetaInLatestVersion)
      _ <- deleteFiles(fileDeletions.toList)

      pathsToAdd = pathToFileInfo.keySet diff pathToFileMetaInLatestVersion.keySet diff oldToNewPathMovedFiles.values.toSet
      fileAdditions <- addFiles(deposit.dataversePid, pathsToAdd.map(pathToFileInfo).toList)

      _ <- updateFileMetadata(fileReplacements ++ fileMovements ++ fileAdditions)
    } yield deposit.dataversePid
  }

  private def getFilesInLatestVersion: Try[Map[Path, FileMeta]] = {
    for {
      response <- dataset.listFiles()
      files <- response.data
      pathToFileMeta = files.map(f => (getPathFromFileMeta(f), f)).toMap
    } yield pathToFileMeta
  }

  private def getPathFromFileMeta(fileMeta: FileMeta): Path = {
    Paths.get(fileMeta.directoryLabel.getOrElse(""), fileMeta.label.getOrElse(""))
  }

  private def validateFileMetas(files: List[FileMeta]): Try[Unit] = {
    if (files.map(_.dataFile).exists(_.isEmpty)) Failure(new IllegalArgumentException("Found file metadata without dataFile element"))
    else if (files.map(_.dataFile.get).exists(_.checksum.`type` != "SHA-1")) Failure(new IllegalArgumentException("Not all file checksums are of type SHA-1"))
         else Success(())
  }

  private def getFilesToReplace(pathToFileInfo: Map[Path, FileInfo], pathToFileMetaInLatestVersion: Map[Path, FileMeta]): Try[Map[Int, FileInfo]] = Try {
    trace(())
    val intersection = pathToFileInfo.keySet intersect pathToFileMetaInLatestVersion.keySet
    val checksumsDiffer = intersection.filter(p => pathToFileInfo(p).checksum != pathToFileMetaInLatestVersion(p).dataFile.get.checksum.value) // TODO: validate filemetas first
    checksumsDiffer.map(p => (pathToFileMetaInLatestVersion(p).dataFile.get.id, pathToFileInfo(p))).toMap
  }

  private def getOldToNewPathOfFilesToMove(pathToFileMetaInLatestVersion: Map[Path, FileMeta], pathToFileInfo: Map[Path, FileInfo]): Try[Map[Path, Path]] = {
    for {
      checksumsToPathNonDuplicatedFilesInDeposit <- getChecksumsToPathOfNonDuplicateFiles(pathToFileInfo.mapValues(_.checksum))
      checksumsToPathNonDuplicatedFilesInLatestVersion <- getChecksumsToPathOfNonDuplicateFiles(pathToFileMetaInLatestVersion.mapValues(_.dataFile.get.checksum.value))
      checksumsOfPotentiallyMovedFiles = checksumsToPathNonDuplicatedFilesInDeposit.keySet intersect checksumsToPathNonDuplicatedFilesInLatestVersion.keySet
      oldToNewPathMovedFiles = checksumsOfPotentiallyMovedFiles
        .map(c => (checksumsToPathNonDuplicatedFilesInLatestVersion(c), checksumsToPathNonDuplicatedFilesInDeposit(c)))
        .filter { case (pathInLatestVersion, pathInDeposit) => pathInLatestVersion != pathInDeposit }
    } yield oldToNewPathMovedFiles.toMap
  }

  private def getChecksumsToPathOfNonDuplicateFiles(pathToChecksum: Map[Path, String]): Try[Map[String, Path]] = Try {
    pathToChecksum
      .groupBy { case (_, c) => c }
      .filter { case (_, pathToFileInfoMappings) => pathToFileInfoMappings.size == 1 }
      .map { case (c, m) => (c, m.head._1) }
  }

  private def getFileDeletions(paths: Set[Path], pathToFileMeta: Map[Path, FileMeta]): Try[Set[Int]] = Try {
    paths.map(path => pathToFileMeta(path).dataFile.get.id)
  }
}
