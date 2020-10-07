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

import nl.knaw.dans.easy.dd2d.dataverse.json.{ CompoundField, DatasetVersion, DataverseDataset, Field, MetadataBlock, PrimitiveFieldMultipleValues, PrimitiveFieldSingleValue, ValueObject, createCompoundFieldMultipleValues }
import nl.knaw.dans.easy.dd2d.mapping._
import org.json4s.DefaultFormats

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.xml.{ Elem, MetaData, Node, NodeSeq }
import scala.language.postfixOps

/**
 * Maps DANS Dataset Metadata to Dataverse JSON.
 */
// TODO: Rename if we also need to take elements from EMD
class DdmToDataverseMapper() {
  case class RelatedIdentifier(relationType: String, schemeOrUrl: String, value: String, isRelatedIdentifier: Boolean)
  lazy val citationFields = new ListBuffer[Field]
  lazy val basicInformationFields = new ListBuffer[Field]
  lazy val archaeologySpecificFields = new ListBuffer[Field]
  lazy val temporalSpatialFields = new ListBuffer[Field]

  object IdScheme extends Enumeration {
    type IdScheme = Value
    val DOI: IdScheme.Value = Value("doi")
    val URN: IdScheme.Value = Value("urn:nbn:nl")
    val ISBN: IdScheme.Value = Value("ISBN")
    val ISSN: IdScheme.Value = Value("ISSN")
    val NWO: IdScheme.Value = Value("NWO ProjectNr")
    val OTHER: IdScheme.Value = Value("other")
    val DEFAULT: IdScheme.Value = Value("")
  }

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

  private def addCompoundFieldMultipleValues(metadataBlockFields: ListBuffer[Field], name: String, sourceNodes: NodeSeq, nodeTransformer: Node => ValueObject): Unit = {
    val valueObjects = new ListBuffer[ValueObject]()
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

  def mapToCompoundFields(node: Node): Try[Unit] = Try {
    addDates(node)
    addDatesFreeFormat(node)
    addRelatedIdentifiers(node)
    addKeywords(node)
  }

  def addKeywords(node: Node): Try[Unit] = Try {
    val objectList = new ListBuffer[Map[String, Field]]()
    val keywords = node \\ "subject"
    if (keywords.nonEmpty) {
      keywords.foreach(subject => {
        var subFields = collection.mutable.Map[String, Field]()
        subFields += ("keywordValue" -> PrimitiveFieldSingleValue("keywordValue", multiple = false, "primitive", subject.text))
        //subFields += ("keywordVocabulary" -> PrimitiveFieldSingleValue("keywordVocabulary", multiple = false, "primitive", "NOT AVAILABLE IN EASY"))
        //subFields += ("keywordVocabularyURI" -> PrimitiveFieldSingleValue("keywordVocabularyURI", multiple = false, "primitive", "NOT AVAILABLE IN EASY"))
        objectList += subFields.toMap
      })
      citationFields += CompoundField("keyword", multiple = true, "compound", objectList.toList)
    }
  }

  def addDates(node: Node): Try[Unit] = Try {
    val objectList = new ListBuffer[Map[String, Field]]()
    val dateElements = (node \\ "_").collect {
      case e @ Elem("dcterms", "dateAccepted", _, _, _) if e.attributes.nonEmpty => ("Date accepted", e.text)
      case e @ Elem("dcterms", "valid", _, _, _) if e.attributes.nonEmpty => ("Valid", e.text)
      case e @ Elem("dcterms", "issued", _, _, _) if e.attributes.nonEmpty => ("Issued", e.text)
      case e @ Elem("dcterms", "modified", _, _, _) if e.attributes.nonEmpty => ("Modified", e.text)
      case e @ Elem("dc", "date", _, _, _) if e.attributes.nonEmpty => ("Date", e.text)
    }
    if (dateElements.nonEmpty) {
      dateElements.foreach(date => {
        var subFields = collection.mutable.Map[String, Field]()
        subFields += ("easy-date-event" -> PrimitiveFieldSingleValue("easy-date-event", multiple = false, "controlledVocabulary", date._1))
        subFields += ("esy-date-val" -> PrimitiveFieldSingleValue("esy-date-val", multiple = false, "primitive", date._2))
        objectList += subFields.toMap
      })
      basicInformationFields += CompoundField("easy-date", multiple = true, "compound", objectList.toList)
    }
  }

  def addDatesFreeFormat(node: Node): Try[Unit] = Try {
    val objectList = new ListBuffer[Map[String, Field]]()
    val dateElements = (node \\ "_").collect {
      case e @ Elem("dcterms", "dateAccepted", _, _, _) if e.attributes.isEmpty => ("Date accepted", e.text)
      case e @ Elem("dcterms", "valid", _, _, _) if e.attributes.isEmpty => ("Valid", e.text)
      case e @ Elem("dcterms", "issued", _, _, _) if e.attributes.isEmpty => ("Issued", e.text)
      case e @ Elem("dcterms", "modified", _, _, _) if e.attributes.isEmpty => ("Modified", e.text)
      case e @ Elem("dc", "date", _, _, _) if e.attributes.isEmpty => ("Date", e.text)
    }
    if (dateElements.nonEmpty) {
      dateElements.foreach(date => {
        var subFields = collection.mutable.Map[String, Field]()
        subFields += ("easy-date-event-free" -> PrimitiveFieldSingleValue("easy-date-event-free", multiple = false, "controlledVocabulary", date._1))
        subFields += ("easy-date-val-free" -> PrimitiveFieldSingleValue("easy-date-val-free", multiple = false, "primitive", date._2))
        objectList += subFields.toMap
      })
      basicInformationFields += CompoundField("easy-date-free", multiple = true, "compound", objectList.toList)
    }
  }

  def addRelatedIdentifiers(node: Node): Try[Unit] = Try {
    val relIdList = new ListBuffer[Map[String, Field]]()
    val relIdUrlList = new ListBuffer[Map[String, Field]]()

    val relationElements = (node \\ "_").collect {
      case e @ Elem(_, "conformsTo", _, _, _) if isRelatedIdentifier(e.attributes) => RelatedIdentifier("relation", mapScheme(e.attributes).toString, e.text, isRelatedIdentifier = true)
      case e @ Elem(_, "relation", _, _, _) if isRelatedIdentifier(e.attributes) => RelatedIdentifier("relation", mapScheme(e.attributes).toString, e.text, isRelatedIdentifier = false)
      case e @ Elem(_, "relation", _, _, _) if !isRelatedIdentifier(e.attributes) => RelatedIdentifier("relation", getUrl(e.attributes), e.text, isRelatedIdentifier = false)
    }
    if (relationElements.nonEmpty) {
      relationElements.foreach(relation => {
        var subFields = collection.mutable.Map[String, Field]()
        if (relation.isRelatedIdentifier) {
          subFields += ("easy-relid-relation" -> PrimitiveFieldSingleValue("easy-relid-relation", multiple = false, "controlledVocabulary", relation.relationType))
          subFields += ("easy-relid-type" -> PrimitiveFieldSingleValue("easy-relid-type", multiple = false, "controlledVocabulary", relation.schemeOrUrl))
          subFields += ("easy-relid-relatedid" -> PrimitiveFieldSingleValue("easy-relid-relatedid", multiple = false, "primitive", relation.value))
          relIdList += subFields.toMap
        }
        else {
          subFields += ("easy-relid-relation-url" -> PrimitiveFieldSingleValue("easy-relid-relation-url", multiple = false, "controlledVocabulary", relation.relationType))
          subFields += ("easy-relid-url-title" -> PrimitiveFieldSingleValue("easy-relid-url-title", multiple = false, "primitive", relation.value))
          subFields += ("easy-relid-url-url" -> PrimitiveFieldSingleValue("easy-relid-url-url", multiple = false, "primitive", relation.schemeOrUrl))
          relIdUrlList += subFields.toMap
        }
      })
      basicInformationFields += CompoundField("easy-relid", multiple = true, "compound", relIdList.toList)
      basicInformationFields += CompoundField("easy-relid-url", multiple = true, "compound", relIdUrlList.toList)
    }
  }

  //check if RelatedIdentifier or Relation
  def isRelatedIdentifier(md: MetaData): Boolean = {
    md.get("xsi-type").nonEmpty || md.get("scheme").nonEmpty
  }

  def mapScheme(md: MetaData): IdScheme.Value = {
    val attr = md.asAttrMap.filter(a => a._1 == "scheme" | (a._1 == "xsi:type"))
    attr.head._2 match {
      case "id-type:NWO-PROJECTNR" => IdScheme.NWO
      case "id-type:ISBN" => IdScheme.ISBN
      case "id-type:ISSN" => IdScheme.ISSN
      case "DOI" | "id-type:DOI" => IdScheme.DOI
      case "URN" => IdScheme.URN
      case "id-type:other" => IdScheme.OTHER
      case _ => IdScheme.DEFAULT
    }
  }

  def getUrl(md: MetaData): String = {
    md.get("href").getOrElse("").asInstanceOf[String]
  }
}
