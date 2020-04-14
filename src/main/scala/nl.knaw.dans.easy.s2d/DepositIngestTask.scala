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

import nl.knaw.dans.easy.s2d.dataverse.DataverseInstance
import nl.knaw.dans.easy.s2d.queue.Task
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.Try

case class DepositIngestTask(deposit: Deposit, dataverse: DataverseInstance) extends Task with DebugEnhancedLogging {
  override def run(): Try[Unit] = {
    trace(())
    debug(s"Ingesting $deposit into Dataverse")

    // TODO: validate directory. Is it really a deposit?

    // Read title from metadata
    val title = "My title"

    // Create dataset

    // Assemble a quick-and-dirty JSON
    val json =
      s"""
         |{
         |  "datasetVersion": {
         |    "metadataBlocks": {
         |      "citation": {
         |        "fields": [
         |          {
         |            "value": "$title",
         |            "typeClass": "primitive",
         |            "multiple": false,
         |            "typeName": "title"
         |          },
         |          {
         |            "value": [
         |              {
         |                "authorName": {
         |                  "value": "Finch, Fiona",
         |                  "typeClass": "primitive",
         |                  "multiple": false,
         |                  "typeName": "authorName"
         |                },
         |                "authorAffiliation": {
         |                  "value": "Birds Inc.",
         |                  "typeClass": "primitive",
         |                  "multiple": false,
         |                  "typeName": "authorAffiliation"
         |                }
         |              }
         |            ],
         |            "typeClass": "compound",
         |            "multiple": true,
         |            "typeName": "author"
         |          },
         |          {
         |            "value": [
         |              { "datasetContactEmail" : {
         |                "typeClass": "primitive",
         |                "multiple": false,
         |                "typeName": "datasetContactEmail",
         |                "value" : "finch@mailinator.com"
         |              },
         |                "datasetContactName" : {
         |                  "typeClass": "primitive",
         |                  "multiple": false,
         |                  "typeName": "datasetContactName",
         |                  "value": "Finch, Fiona"
         |                }
         |              }],
         |            "typeClass": "compound",
         |            "multiple": true,
         |            "typeName": "datasetContact"
         |          },
         |          {
         |            "value": [ {
         |              "dsDescriptionValue":{
         |                "value":   "Darwin's finches (also known as the Galápagos finches) are a group of about fifteen species of passerine birds.",
         |                "multiple":false,
         |                "typeClass": "primitive",
         |                "typeName": "dsDescriptionValue"
         |              }}],
         |            "typeClass": "compound",
         |            "multiple": true,
         |            "typeName": "dsDescription"
         |          },
         |          {
         |            "value": [
         |              "Medicine, Health and Life Sciences"
         |            ],
         |            "typeClass": "controlledVocabulary",
         |            "multiple": true,
         |            "typeName": "subject"
         |          }
         |        ],
         |        "displayName": "Citation Metadata"
         |      }
         |    }
         |  }
         |}
         |""".stripMargin

    dataverse.dataverse("root").createDataset(json).map(_ => ())
  }
}
