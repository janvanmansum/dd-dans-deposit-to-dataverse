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
import nl.knaw.dans.easy.dd2d.dansbag.DansBagValidator
import nl.knaw.dans.easy.dd2d.mapping.Amd
import nl.knaw.dans.lib.dataverse.DataverseInstance
import nl.knaw.dans.lib.dataverse.model.dataset.Dataset

import scala.language.postfixOps
import scala.util.{ Failure, Success, Try }
import scala.xml.{ Elem, Node }

class DepositMigrationTask(deposit: Deposit,
                           activeMetadataBlocks: List[String],
                           dansBagValidator: DansBagValidator,
                           instance: DataverseInstance,
                           publishAwaitUnlockMaxNumberOfRetries: Int,
                           publishAwaitUnlockMillisecondsBetweenRetries: Int,
                           narcisClassification: Elem,
                           isoToDataverseLanguage: Map[String, String],
                           repordIdToTerm: Map[String, String],
                           outboxDir: File)
  extends DepositIngestTask(deposit: Deposit,
    activeMetadataBlocks: List[String],
    dansBagValidator: DansBagValidator,
    instance: DataverseInstance,
    publishAwaitUnlockMaxNumberOfRetries: Int,
    publishAwaitUnlockMillisecondsBetweenRetries: Int,
    narcisClassification: Elem,
    isoToDataverseLanguage: Map[String, String],
    repordIdToTerm: Map[String, String],
    outboxDir: File) {

  override protected def checkDepositType(): Try[Unit] = {
    for {
      _ <- if (deposit.doi.isEmpty) Failure(new IllegalArgumentException("Deposit for migrated dataset MUST have deposit property identifier.doi set"))
           else Success(())
      _ <- deposit.vaultMetadata.checkMinimumFieldsForImport()
    } yield ()
  }

  override def newDatasetCreator(dataverseDataset: Dataset): DatasetCreator = {
    new DatasetCreator(deposit, isMigration = true, dataverseDataset, instance)
  }

  override protected def getDateOfDeposit: Try[Option[String]] = {
    for {
      optAmd <- deposit.tryOptAmd
      optDate = optAmd.flatMap(Amd toDateOfDeposit)
    } yield optDate
  }

  override protected def publishDataset(persistentId: String): Try[Unit] = {
    trace(persistentId)
    for {
      optAmd <- deposit.tryOptAmd
      optPublicationDate <- getJsonLdPublicationdate(optAmd)
      _ <- instance.dataset(persistentId).releaseMigrated(optPublicationDate.get)
      _ <- instance.dataset(persistentId).awaitUnlock(
        maxNumberOfRetries = publishAwaitUnlockMaxNumberOfRetries,
        waitTimeInMilliseconds = publishAwaitUnlockMillisecondsBetweenRetries)
    } yield ()
  }

  private def getJsonLdPublicationdate(optAmd: Option[Node]): Try[Option[String]] = Try {
    trace(optAmd)
    optAmd
      .flatMap(amd => Amd.getFirstChangeToState(amd, "PUBLISHED"))
      .map(d => s"""{"http://schema.org/datePublished": "$d"}""")
  }

  override protected def postPublication(persistentId: String): Try[Unit] = {
    trace(persistentId)
    Success(())
  }
}
