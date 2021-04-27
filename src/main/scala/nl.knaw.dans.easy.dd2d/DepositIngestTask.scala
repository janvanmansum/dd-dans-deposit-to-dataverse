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
import nl.knaw.dans.easy.dd2d.OutboxSubdir.{ FAILED, OutboxSubdir, PROCESSED, REJECTED }
import nl.knaw.dans.easy.dd2d.dansbag.{ DansBagValidationResult, DansBagValidator }
import nl.knaw.dans.easy.dd2d.mapping.{ Amd, JsonObject }
import nl.knaw.dans.lib.dataverse.DataverseInstance
import nl.knaw.dans.lib.dataverse.model.dataset.UpdateType.major
import nl.knaw.dans.lib.dataverse.model.dataset.{ PrimitiveSingleValueField, toFieldMap }
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.taskqueue.Task
import org.json4s.{ DefaultFormats, Formats }
import org.json4s.native.Serialization

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.util.Try
import scala.util.control.NonFatal
import scala.xml.{ Elem, Node }

/**
 * Checks one deposit and then ingests it into Dataverse.
 *
 * @param deposit  the deposit to ingest
 * @param instance the Dataverse instance to ingest in
 */
case class DepositIngestTask(deposit: Deposit,
                             activeMetadataBlocks: List[String],
                             dansBagValidator: DansBagValidator,
                             instance: DataverseInstance,
                             publishAwaitUnlockMaxNumberOfRetries: Int,
                             publishAwaitUnlockMillisecondsBetweenRetries: Int,
                             narcisClassification: Elem,
                             isoToDataverseLanguage: Map[String, String],
                             repordIdToTerm: Map[String, String],
                             outboxDir: File) extends Task[Deposit] with DebugEnhancedLogging {
  trace(deposit)

  private val datasetMetadataMapper = new DepositToDvDatasetMetadataMapper(activeMetadataBlocks, narcisClassification, isoToDataverseLanguage, repordIdToTerm)
  private val bagDirPath = File(deposit.bagDir.path)

  override def run(): Try[Unit] = doRun()
    .doIfSuccess(_ => {
      deposit.setState("ARCHIVED", "The deposit was successfully ingested in the Data Station and will be automatically archived")
      moveDepositToOutbox(PROCESSED)
    })
    .doIfFailure {
      case e: RejectedDepositException =>
        deposit.setState("REJECTED", e.msg)
        moveDepositToOutbox(REJECTED)
      case e =>
        deposit.setState("FAILED", e.getMessage)
        moveDepositToOutbox(FAILED)
    }

  private def doRun(): Try[Unit] = {
    trace(())
    logger.info(s"Ingesting $deposit into Dataverse")
    for {
      validationResult <- dansBagValidator.validateBag(bagDirPath)
      _ <- rejectIfInvalid(validationResult)
      response <- instance.admin().getSingleUser(deposit.depositorUserId)
      user <- response.data
      datasetContacts <- createDatasetContacts(user.displayName, user.email, user.affiliation)
      ddm <- deposit.tryDdm
      optAgreements <- deposit.tryOptAgreementsXml
      optAmd <- deposit.tryOptAmd
      dataverseDataset <- datasetMetadataMapper.toDataverseDataset(ddm, optAgreements, optAmd, datasetContacts, deposit.vaultMetadata)
      isUpdate <- deposit.isUpdate
      _ = debug(s"isUpdate? = $isUpdate")
      editor = if (isUpdate) new DatasetUpdater(deposit, dataverseDataset.datasetVersion.metadataBlocks, instance)
               else new DatasetCreator(deposit, dataverseDataset, instance)
      persistentId <- editor.performEdit()
      publicationDateOpt <- getJsonLdPublicationdate(optAmd)
      _ <- publishDataset(persistentId, publicationDateOpt)
      _ <- savePersistentIdentifiersInDepositProperties(persistentId)
    } yield ()
  }

  private def getJsonLdPublicationdate(optAmd: Option[Node]): Try[Option[String]] = Try {
    trace(optAmd)
    optAmd
      .flatMap(amd => Amd.getFirstChangeToState(amd, "PUBLISHED"))
      .map(d => s"""{"http://schema.org/datePublished": "$d"}""")
  }

  private def moveDepositToOutbox(subDir: OutboxSubdir): Unit = {
    try {
      deposit.dir.moveToDirectory(outboxDir / subDir.toString)
    } catch {
      case NonFatal(e) => logger.info(s"Failed to move deposit: $deposit to ${ outboxDir / subDir.toString }", e)
    }
  }

  private def rejectIfInvalid(validationResult: DansBagValidationResult): Try[Unit] = Try {
    if (!validationResult.isCompliant) throw RejectedDepositException(deposit,
      s"""
         |Bag was not valid according to Profile Version ${ validationResult.profileVersion }.
         |Violations:
         |${ validationResult.ruleViolations.map(_.map(formatViolation).mkString("\n")).getOrElse("") }
                      """.stripMargin)
  }

  private def formatViolation(v: (String, String)): String = v match {
    case (nr, msg) => s" - [$nr] $msg"
  }

  private def createDatasetContacts(name: String, email: String, optAffiliation: Option[String] = None): Try[List[JsonObject]] = Try {
    val subfields = ListBuffer[PrimitiveSingleValueField]()
    subfields.append(PrimitiveSingleValueField("datasetContactName", name))
    subfields.append(PrimitiveSingleValueField("datasetContactEmail", email))
    optAffiliation.foreach(affiliation => subfields.append(PrimitiveSingleValueField("datasetContactAffiliation", affiliation)))
    List(toFieldMap(subfields: _*))
  }

  private def publishDataset(persistentId: String, publicationDateOpt: Option[String]): Try[Unit] = {
    trace(persistentId, publicationDateOpt)
    for {
      _ <- publicationDateOpt match {
        case Some(publicationDate) => instance.dataset(persistentId).releaseMigrated(publicationDate)
        case None => instance.dataset(persistentId).publish(major)
      }
      _ <- instance.dataset(persistentId).awaitUnlock(
        maxNumberOfRetries = publishAwaitUnlockMaxNumberOfRetries,
        waitTimeInMilliseconds = publishAwaitUnlockMillisecondsBetweenRetries)
    } yield ()
  }

  private def savePersistentIdentifiersInDepositProperties(persistentId: String): Try[Unit] = {
    implicit val jsonFormats: Formats = DefaultFormats
    for {
      _ <- instance.dataset(persistentId).awaitUnlock()
      _ = debug(s"Dataset $persistentId is not locked")
      _ <- deposit.setDoi(persistentId)
      // TODO: try several times if the vault metadata don't appear immediately
      r <- instance.dataset(persistentId).view()
      _ = if(logger.underlying.isDebugEnabled) debug(Serialization.writePretty(r.json))
      d <- r.data
      v = d.metadataBlocks("dansDataVaultMetadata")
      optUrn = v.fields.find(_.typeName == "dansNbn")
        .map(_.asInstanceOf[PrimitiveSingleValueField])
        .map(_.value)
      _ = if (optUrn.isEmpty) throw new IllegalStateException(s"Dataset $persistentId did not obtain a URN:NBN")
      _ <- deposit.setUrn(optUrn.get)
    } yield ()
  }

  override def getTarget: Deposit = {
    deposit
  }

  override def toString: DepositName = {
    s"DepositIngestTask for ${ deposit }"
  }
}
