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

import nl.knaw.dans.easy.dd2d.dataverse.json.{ DatasetVersion, DataverseDataset, Field, JsonObject, MetadataBlock, PrimitiveFieldMultipleValues, PrimitiveFieldSingleValue, createCompoundFieldMultipleValues }
import nl.knaw.dans.easy.dd2d.mapping._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.util.Try
import scala.xml.{ Node, NodeSeq }

/**
 * Maps DANS Dataset Metadata to Dataverse JSON.
 */
// TODO: Rename if we also need to take elements from EMD
class DdmToDataverseMapper() {
  lazy val citationFields = new ListBuffer[Field]
  lazy val basicInformationFields = new ListBuffer[Field]
  lazy val archaeologySpecificFields = new ListBuffer[Field]
  lazy val temporalSpatialFields = new ListBuffer[Field]

  def toDataverseDataset(ddm: Node): Try[DataverseDataset] = Try {
    // Please keep ordered by order in Dataverse UI as much as possible

    // TODO: if a single value is expected, the first encountered will be used; is this OK? Add checks on multiplicity before processing?

    // Citation
    addPrimitiveFieldSingleValue(citationFields, "title", ddm \ "profile" \ "title")
    addCompoundFieldMultipleValues(citationFields, "otherId", ddm \ "dcmiMetadata" \ "isFormatOf", IsFormatOf toOtherIdValueObject)
    addCompoundFieldMultipleValues(citationFields, "dsDescription", ddm \ "profile" \ "description", Description toDescriptionValueObject)
    addCompoundFieldMultipleValues(citationFields, "author", ddm \ "profile" \ "creatorDetails" \ "author", DcxDaiAuthor toAuthorValueObject)
    // TODO: creator unstructured
    addPrimitiveFieldSingleValue(citationFields, "productionDate", ddm \ "profile" \ "created", DateTypeElement toYearMonthDayFormat)
    addPrimitiveFieldSingleValue(citationFields, "distributionDate", ddm \ "profile" \ "available", DateTypeElement toYearMonthDayFormat)
    addCvFieldMultipleValues(citationFields, "subject", ddm \ "profile" \ "audience", Audience toCitationBlockSubject)
    addCvFieldMultipleValues(citationFields, "language", ddm \ "dcmiMetadata" \ "language", Language toCitationBlockLanguage)
    addPrimitiveFieldSingleValue(citationFields, "alternativeTitle", ddm \ "dcmiMetadata" \ "alternative")
    addPrimitiveFieldMultipleValues(citationFields, "dataSources", ddm \ "dcmiMetadata" \ "source")
    addCompoundFieldMultipleValues(citationFields, "contributor", ddm \ "dcmiMetadata" \ "contributorDetails" \ "author", DcxDaiAuthor toContributorValueObject)
    addCompoundFieldMultipleValues(citationFields, "contributor", ddm \ "profile" \ "creatorDetails" \ "organization", DcxDaiOrganization toContributorValueObject)
    addCompoundFieldMultipleValues(citationFields, "contributor", ddm \ "dcmiMetadata" \ "contributorDetails" \ "organization", DcxDaiOrganization toContributorValueObject)
    // TODO: contributor unstructured

    // Basic information
    addPrimitiveFieldMultipleValues(basicInformationFields, "languageOfFiles", ddm \ "dcmiMetadata" \ "language")
    addCompoundFieldMultipleValues(basicInformationFields, "easy-relid", (ddm \ "dcmiMetadata" \ "_").filter(Relation isRelation).filter(Relation isRelatedIdentifier), Relation toRelatedIdentifierValueObject)
    addCompoundFieldMultipleValues(basicInformationFields, "easy-relid-url", (ddm \ "dcmiMetadata" \ "_").filter(Relation isRelation).filterNot(Relation isRelatedIdentifier), Relation toRelatedUrlValueObject)
    addCompoundFieldMultipleValues(basicInformationFields, "easy-date", (ddm \ "dcmiMetadata" \ "_").filter(DateTypeElement isDate).filter(DateTypeElement hasW3CFormat), DateTypeElement toBasicInfoFormattedDateValueObject)
    addCompoundFieldMultipleValues(basicInformationFields, "easy-date-free", (ddm \ "dcmiMetadata" \ "_").filter(DateTypeElement isDate).filterNot(DateTypeElement hasW3CFormat), DateTypeElement toBasicInfoFreeDateValue)

    // Archaeology specific
    addPrimitiveFieldMultipleValues(archaeologySpecificFields, "archisZaakId", ddm \ "dcmiMetadata" \ "identifier", IsFormatOf toArchisZaakId)

    // Temporal and spatial coverage
    addCompoundFieldMultipleValues(temporalSpatialFields, "easy-tsm", ddm \ "dcmiMetadata" \ "spatial" \ "Point", SpatialPoint toEasyTsmSpatialPointValueObject)
    addCompoundFieldMultipleValues(temporalSpatialFields, "easy-spatial-box", ddm \ "dcmiMetadata" \ "spatial" \ "boundedBy", SpatialBox toEasyTsmSpatialBoxValueObject)

    assembleDataverseDataset()
  }

  private def assembleDataverseDataset(): DataverseDataset = {
    val versionMap = mutable.Map[String, MetadataBlock]()
    addMetadataBlock(versionMap, "citation", "Citation Metadata", citationFields)
    addMetadataBlock(versionMap, "basicInformation", "Basic Information", basicInformationFields)
    addMetadataBlock(versionMap, "archaeologyMetadata", "Archaeology-Specific Metadata", archaeologySpecificFields)
    addMetadataBlock(versionMap, "temporal-spatial", "Temporal and Spatial Coverage", temporalSpatialFields)
    val datasetVersion = DatasetVersion(versionMap.toMap)
    DataverseDataset(datasetVersion)
  }

  private def addPrimitiveFieldSingleValue(metadataBlockFields: ListBuffer[Field], name: String, sourceNodes: NodeSeq, nodeTransformer: Node => Option[String] = AnyElement toText): Unit = {
    sourceNodes
      .map(nodeTransformer)
      .filter(_.isDefined)
      .map(_.get)
      .take(1)
      .foreach(v => metadataBlockFields += PrimitiveFieldSingleValue(name, multiple = false, "primitive", v))
  }

  private def addPrimitiveFieldMultipleValues(metadataBlockFields: ListBuffer[Field], name: String, sourceNodes: NodeSeq, nodeTransformer: Node => Option[String] = AnyElement toText): Unit = {
    val values = sourceNodes.map(nodeTransformer).filter(_.isDefined).map(_.get).toList
    if (values.nonEmpty) {
      metadataBlockFields += PrimitiveFieldMultipleValues(name, multiple = true, "primitive", values)
    }
  }

  private def addCvFieldMultipleValues(metadataBlockFields: ListBuffer[Field], name: String, sourceNodes: NodeSeq, nodeTransformer: Node => Option[String]): Unit = {
    val values = sourceNodes.map(nodeTransformer).filter(_.isDefined).map(_.get).toList
    if (values.nonEmpty) {
      metadataBlockFields += PrimitiveFieldMultipleValues(name, multiple = true, "controlledVocabulary", values)
    }
  }

  private def addCompoundFieldMultipleValues(metadataBlockFields: ListBuffer[Field], name: String, sourceNodes: NodeSeq, nodeTransformer: Node => JsonObject): Unit = {
    val valueObjects = new ListBuffer[JsonObject]()
    sourceNodes.foreach(e => valueObjects += nodeTransformer(e))
    if (valueObjects.nonEmpty) {
      metadataBlockFields += createCompoundFieldMultipleValues(name, valueObjects.toList)
    }
  }

  private def addMetadataBlock(versionMap: mutable.Map[String, MetadataBlock], blockId: String, blockDisplayName: String, fields: ListBuffer[Field]): Unit = {
    if (fields.nonEmpty) {
      versionMap.put(blockId, MetadataBlock(blockDisplayName, fields.toList))
    }
  }
}
