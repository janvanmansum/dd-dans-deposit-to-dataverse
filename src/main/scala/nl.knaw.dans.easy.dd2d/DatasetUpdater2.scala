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
import gov.loc.repository.bagit.hash.StandardSupportedAlgorithms
import nl.knaw.dans.lib.dataverse.DataverseInstance
import nl.knaw.dans.lib.dataverse.model.dataset.MetadataBlocks
import nl.knaw.dans.lib.dataverse.model.file.FileMeta
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import java.lang.Thread.sleep
import java.nio.file.{ Path, Paths }
import scala.collection.JavaConverters.{ asScalaSetConverter, mapAsScalaMapConverter }
import scala.util.{ Failure, Success, Try }

class DatasetUpdater2(deposit: Deposit, metadataBlocks: MetadataBlocks, instance: DataverseInstance) extends DatasetEditor(deposit, instance) with DebugEnhancedLogging {
  trace(deposit)

  override def performEdit(): Try[PersistendId] = {
    for {
      _ <- Try { sleep(1000) } // Temporary stop-gap to avoid random behavior from Dataverse

      _ <- dataset.awaitUnlock()
      _ <- dataset.updateMetadata(metadataBlocks)

      pathToFileInfo <- deposit.getPathToFileInfo
      checksumToFileInfoInDeposit <- getFilesInDeposit(pathToFileInfo)
      pathToFileMetaInLatestVersion <- getFilesInLatestVersion2

      checksumToFileMetaInLatestVersion <- getFilesInLatestVersion

      // Replace file data
      fileReplacements <- getFilesToReplace2(pathToFileInfo, pathToFileMetaInLatestVersion)
      _ <- replaceFiles2(fileReplacements)

      // Moved files
      checksumsToPathNonDuplicatedFilesInDeposit = getChecksumsToPathOfNonDuplicateFiles(pathToFileInfo.mapValues(_.checksum))
      checksumsToPathNonDuplicatedFilesInLatestVersion = getChecksumsToPathOfNonDuplicateFiles(pathToFileMetaInLatestVersion.mapValues(_.dataFile.get.checksum.value))
      checksumsOfPotentiallyMovedFiles = checksumsToPathNonDuplicatedFilesInDeposit.keySet intersect checksumsToPathNonDuplicatedFilesInLatestVersion.keySet
      oldToNewPathMovedFiles = checksumsOfPotentiallyMovedFiles
        .map(c => (checksumsToPathNonDuplicatedFilesInLatestVersion(c), checksumsToPathNonDuplicatedFilesInDeposit(c)))
        .filter { case (pathInLatestVersion, pathInDeposit) => pathInLatestVersion != pathInDeposit }

      databaseIdsMovedFiles = oldToNewPathMovedFiles.map { case (old, _) => pathToFileMetaInLatestVersion(old).dataFile.get.id }

      // Delete files
      pathsToDelete = pathToFileMetaInLatestVersion.keySet diff pathToFileInfo.keySet diff oldToNewPathMovedFiles.map { case (old, _) => old }
      fileDeletions <- getFileDeletions(pathsToDelete, pathToFileMetaInLatestVersion)
      _ <- deleteFiles(fileDeletions.toList)

      // Add file data
      pathsToAdd = pathToFileInfo.keySet diff pathToFileMetaInLatestVersion.keySet diff oldToNewPathMovedFiles.map { case (_, newPath) => newPath }

      // Update file metadata

      // (old, new) checksum
      checksumPairsToReplace <- getFilesToReplace(checksumToFileInfoInDeposit, checksumToFileMetaInLatestVersion)
      checksumReplacedFiles = checksumPairsToReplace.map(_._1)
      checksumReplacementFiles = checksumPairsToReplace.map(_._2)
      _ <- logFilesToReplace(checksumReplacedFiles, checksumToFileMetaInLatestVersion)
      _ <- replaceFiles(checksumPairsToReplace, checksumToFileMetaInLatestVersion, checksumToFileInfoInDeposit)

      checksumsFilesToDelete = (checksumToFileMetaInLatestVersion.keySet diff checksumReplacedFiles.toSet) diff checksumToFileInfoInDeposit.keySet
      _ <- logFilesToDelete(checksumsFilesToDelete.toList, checksumToFileMetaInLatestVersion)
      _ <- deleteFiles(checksumsFilesToDelete.map(checksumToFileMetaInLatestVersion).map(_.dataFile.get.id).toList)

      checksumsFilesToAdd = (checksumToFileInfoInDeposit.keySet diff checksumReplacementFiles.toSet) diff checksumToFileMetaInLatestVersion.keySet
      _ <- logFilesToAdd(checksumsFilesToAdd.toList, checksumToFileInfoInDeposit)
      _ <- addFiles(deposit.dataversePid, checksumsFilesToAdd.map(checksumToFileInfoInDeposit).toList)
    } yield deposit.dataversePid
  }

  //  private def getChecksumsOfNonDuplicateFiles(pathToFileInfo: Map[Path, FileInfo]): Set[String] = {
  //    pathToFileInfo
  //      .groupBy { case (_, f) => f.checksum }
  //      .filter { case (_, pathToFileInfoMappings) => pathToFileInfoMappings.size == 1 }
  //      .keySet
  //  }
  //
  //  private def getChecksumsOfNonDuplicateFiles(pathToFileInfo: Map[Path, FileMeta]): Set[String] = {
  //    pathToFileInfo
  //      .groupBy { case (_, f) => f.dataFile.get.checksum.value }
  //      .filter { case (_, pathToFileInfoMappings) => pathToFileInfoMappings.size == 1 }
  //      .keySet
  //  }

  private def getChecksumsToPathOfNonDuplicateFiles(pathToChecksum: Map[Path, String]): Map[String, Path] = {
    pathToChecksum
      .groupBy { case (_, c) => c }
      .filter { case (_, pathToFileInfoMappings) => pathToFileInfoMappings.size == 1 }
      .map { case (c, m) => (c, m.head._1) }
  }

  /**
   * Creates a map from SHA-1 hash to FileInfo for the files in the payload of the deposited bag.
   *
   * @param pathToFileInfo map from bag-local path to FileInfo
   * @return
   */
  private def getFilesInDeposit(pathToFileInfo: Map[Path, FileInfo]): Try[Map[Sha1Hash, FileInfo]] = {
    for {
      bag <- deposit.tryBag
      optSha1Manifest = bag.getPayLoadManifests.asScala.find(_.getAlgorithm == StandardSupportedAlgorithms.SHA1)
      _ = if (optSha1Manifest.isEmpty) throw new IllegalArgumentException("Deposit bag does not have SHA-1 payload manifest")
      sha1ToFilePath = optSha1Manifest.get.getFileToChecksumMap.asScala.map { case (p, c) => (c, deposit.bagDir.path relativize p) }
      sha1ToFileInfo = sha1ToFilePath.map { case (sha1, path) => (sha1 -> pathToFileInfo(path)) }.toMap // TODO: Note this will erase duplicate files in a dataset
    } yield sha1ToFileInfo
  }

  /**
   * Creates a map from SHA-1 hash to FileMeta for all the files in the latest version of the dataset.
   *
   * @return
   */
  private def getFilesInLatestVersion: Try[Map[Sha1Hash, FileMeta]] = {
    for {
      response <- dataset.listFiles()
      files <- response.data
      _ <- validateFileMetas(files)
      checksumToFileMeta = files.map(f => (f.dataFile.get.checksum.value, f)).toMap
    } yield checksumToFileMeta
  }

  private def getFilesInLatestVersion2: Try[Map[Path, FileMeta]] = {
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

  /**
   * Calculates which files have to be replaced and returns a list of SHA-1 has pairs, the first of which is the old hash and the second the hash of the replacement.
   * Files which have the same (directoryLabel, label) pair in the deposit and in the latest version of the dataset are marked as files to be replaced.
   *
   * @param checksumToFileInfoInDeposit       map from SHA-1 hash to FileInfo for each payload file in the deposited bag
   * @param checksumToFileMetaInLatestVersion map from SHA-1 hash to FileMeta for each file in the latest version of the dataset
   * @return
   */
  private def getFilesToReplace(checksumToFileInfoInDeposit: Map[Sha1Hash, FileInfo], checksumToFileMetaInLatestVersion: Map[Sha1Hash, FileMeta]): Try[List[(Sha1Hash, Sha1Hash)]] = {
    for {
      labelPairToChecksumDeposit <- Try { checksumToFileInfoInDeposit.map { case (c, fi) => ((fi.metadata.directoryLabel, fi.metadata.label) -> c) } }
      labelPairToChecksumLatestVersion <- Try { checksumToFileMetaInLatestVersion.map { case (c, m) => ((m.directoryLabel, m.label) -> c) } }
      intersection = labelPairToChecksumDeposit.keySet intersect labelPairToChecksumLatestVersion.keySet
      checksumsDiffer = intersection.filter(p => labelPairToChecksumDeposit(p) != labelPairToChecksumLatestVersion(p))
      toBeReplaced = checksumsDiffer.map(p => (labelPairToChecksumLatestVersion(p), labelPairToChecksumDeposit(p))).toList
    } yield toBeReplaced
  }

  private def getFilesToReplace2(pathToFileInfo: Map[Path, FileInfo], pathToFileMetaInLatestVersion: Map[Path, FileMeta]): Try[Map[Int, File]] = Try {
    val intersection = pathToFileInfo.keySet intersect pathToFileMetaInLatestVersion.keySet
    val checksumsDiffer = intersection.filter(p => pathToFileInfo(p).checksum != pathToFileMetaInLatestVersion(p).dataFile.get.checksum.value) // TODO: validate filemetas first
    checksumsDiffer.map(p => (pathToFileMetaInLatestVersion(p).dataFile.get.id, pathToFileInfo(p).file)).toMap
  }

  private def getFileDeletions(paths: Set[Path], pathToFileMeta: Map[Path, FileMeta]): Try[Set[Int]] = Try {
    paths.map(path => pathToFileMeta(path).dataFile.get.id)
  }

  /*
   * Utility function for logging.
   */

  private def logFilesToReplace(checksums: List[Sha1Hash], checksumToFileMeta: Map[Sha1Hash, FileMeta]): Try[Unit] = Try {
    if (logger.underlying.isDebugEnabled) debugFiles("Files to replace", checksums.map(checksumToFileMeta))
    else Success(())
  }

  private def logFilesToAdd(checksums: List[Sha1Hash], checksumToFileInfo: Map[Sha1Hash, FileInfo]): Try[Unit] = Try {
    if (logger.underlying.isDebugEnabled) debugFiles("Files to add", checksums.map(checksumToFileInfo).map(_.metadata).toList)
    else Success(())
  }

  private def logFilesToDelete(checksums: List[Sha1Hash], checksumToFileMeta: Map[Sha1Hash, FileMeta]): Try[Unit] = Try {
    if (logger.underlying.isDebugEnabled) debugFiles("Files to delete", checksums.map(checksumToFileMeta).toList)
    else Success(())
  }

  private def debugFiles(prefix: String, files: List[FileMeta]): Unit = {
    debug(s"$prefix: ${ files.map(f => f.directoryLabel.getOrElse("/") + f.label.getOrElse("")).mkString(", ") }")
  }
}
