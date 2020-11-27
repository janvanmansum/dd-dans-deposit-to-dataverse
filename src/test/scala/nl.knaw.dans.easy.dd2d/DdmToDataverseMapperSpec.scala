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

import nl.knaw.dans.easy.dd2d.dataverse.json.{ CompoundField, DatasetVersion, DataverseDataset, MetadataBlock, createPrimitiveFieldSingleValue }
import nl.knaw.dans.easy.dd2d.mapping.{ BlockBasicInformation, BlockCitation, BlockTemporalAndSpatial }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.json4s.JsonAST.JString
import org.json4s.native.Serialization
import org.json4s.{ DefaultFormats, Extraction, JArray, JBool, JValue }

import scala.util.Success

class DdmToDataverseMapperSpec extends TestSupportFixture with DebugEnhancedLogging with BlockCitation with BlockBasicInformation with BlockTemporalAndSpatial {

  implicit val format: DefaultFormats.type = DefaultFormats
  private val mapper = new DdmToDataverseMapper()

  "toDataverseDataset" should "map profile/title to citation/title" in {
    val ddm =
      <ddm:DDM>
        <ddm:profile>
           <dc:title>A title</dc:title>
        </ddm:profile>
        <ddm:dcmiMetadata>
        </ddm:dcmiMetadata>
      </ddm:DDM>

    val result = mapper.toDataverseDataset(ddm)
    result shouldBe a[Success[_]]
    inside(result) {
      case Success(DataverseDataset(DatasetVersion(metadataBlocks))) =>
        metadataBlocks.get("citation") shouldBe Some(
          MetadataBlock("Citation Metadata", List(createPrimitiveFieldSingleValue("title", "A title")))
        )
    }
  }

  it should "map profile/descriptions to citation/descriptions" in {
    val ddm =
      <ddm:DDM>
        <ddm:profile>
           <dc:title>A title</dc:title>
           <dc:description>Descr 1</dc:description>
           <dc:description>Descr 2</dc:description>
        </ddm:profile>
        <ddm:dcmiMetadata>
        </ddm:dcmiMetadata>
      </ddm:DDM>

    val result = mapper.toDataverseDataset(ddm)
    result shouldBe a[Success[_]]
    inside(result) {
      case Success(DataverseDataset(DatasetVersion(metadataBlocks))) =>
        metadataBlocks("citation").fields should contain(
          CompoundField("dsDescription",
            multiple = true,
            "compound",
            (List(
              Map("dsDescriptionValue" -> createPrimitiveFieldSingleValue("dsDescriptionValue", "Descr 1")),
              Map("dsDescriptionValue" -> createPrimitiveFieldSingleValue("dsDescriptionValue", "Descr 2"))
            )
              )))
    }
  }

  it should "map profile/creatorDetails to citation/author" in {
    val ddm =
      <ddm:DDM>
          <ddm:profile>
              <dc:title>A title</dc:title>
              <dcx-dai:creatorDetails>
                  <dcx-dai:author>
                      <dcx-dai:titles>Dr</dcx-dai:titles>
                      <dcx-dai:initials>A</dcx-dai:initials>
                      <dcx-dai:insertions>van</dcx-dai:insertions>
                      <dcx-dai:surname>Helsing</dcx-dai:surname>
                      <dcx-dai:organization>
                          <dcx-dai:name xml:lang="en">Anti-Vampire League</dcx-dai:name>
                      </dcx-dai:organization>
                  </dcx-dai:author>
              </dcx-dai:creatorDetails>
              <dcx-dai:creatorDetails>
                  <dcx-dai:author>
                      <dcx-dai:titles>Professor</dcx-dai:titles>
                      <dcx-dai:initials>T</dcx-dai:initials>
                      <dcx-dai:insertions></dcx-dai:insertions>
                      <dcx-dai:surname>Zonnebloem</dcx-dai:surname>
                      <dcx-dai:organization>
                          <dcx-dai:name xml:lang="en">Uitvindersgilde</dcx-dai:name>
                      </dcx-dai:organization>
                  </dcx-dai:author>
              </dcx-dai:creatorDetails>
          </ddm:profile>
          <ddm:dcmiMetadata>
          </ddm:dcmiMetadata>
      </ddm:DDM>

    val result = mapper.toDataverseDataset(ddm)
    result shouldBe a[Success[_]]
    inside(result) {
      case Success(DataverseDataset(DatasetVersion(metadataBlocks))) =>
        val valueObjectsOfCompoundFields = metadataBlocks("citation").fields.filter(_.isInstanceOf[CompoundField]).map(_.asInstanceOf[CompoundField]).flatMap(_.value)
        valueObjectsOfCompoundFields should contain(
          Map(
            "authorName" -> createPrimitiveFieldSingleValue("authorName", "Dr A van Helsing"),
            "authorAffiliation" -> createPrimitiveFieldSingleValue("authorAffiliation", "Anti-Vampire League")
          ))
        valueObjectsOfCompoundFields should contain(
          Map(
            "authorName" -> createPrimitiveFieldSingleValue("authorName", "Professor T Zonnebloem"),
            "authorAffiliation" -> createPrimitiveFieldSingleValue("authorAffiliation", "Uitvindersgilde")
          ))
    }
  }

  it should "map the whole ddm correctly to JSON objects" in {
    val ddm =
      <ddm:DDM xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" >
          <ddm:profile>
              <dc:title>A title</dc:title>
              <dc:description>Description</dc:description>
              <dc:description>Description 2</dc:description>
              <dcx-dai:creatorDetails>
                  <dcx-dai:author>
                      <dcx-dai:titles>Dr</dcx-dai:titles>
                      <dcx-dai:initials>A</dcx-dai:initials>
                      <dcx-dai:insertions>van</dcx-dai:insertions>
                      <dcx-dai:surname>Helsing</dcx-dai:surname>
                      <dcx-dai:organization>
                          <dcx-dai:name xml:lang="en">Anti-Vampire League</dcx-dai:name>
                      </dcx-dai:organization>
                  </dcx-dai:author>
              </dcx-dai:creatorDetails>
              <dcx-dai:creatorDetails>
                  <dcx-dai:author>
                      <dcx-dai:titles>Professor</dcx-dai:titles>
                      <dcx-dai:initials>T</dcx-dai:initials>
                      <dcx-dai:insertions></dcx-dai:insertions>
                      <dcx-dai:surname>Zonnebloem</dcx-dai:surname>
                      <dcx-dai:organization>
                          <dcx-dai:name xml:lang="en">Uitvindersgilde</dcx-dai:name>
                      </dcx-dai:organization>
                  </dcx-dai:author>
              </dcx-dai:creatorDetails>
              <dcx-dai:creatorDetails>
                  <dcx-dai:organization>
                      <dcx-dai:name xml:lang="en">Anti-Vampire League</dcx-dai:name>
                      <dcx-dai:role xml:lang="en">DataCurator</dcx-dai:role>
                  </dcx-dai:organization>
              </dcx-dai:creatorDetails>
              <ddm:created>2015-09-09</ddm:created>
              <ddm:available>2016-09-08</ddm:available>
              <ddm:audience>D16</ddm:audience>
              <ddm:audience>D1630</ddm:audience>
              <ddm:accessRights>NO_ACCESS</ddm:accessRights>
          </ddm:profile>
          <ddm:dcmiMetadata>
              <dcterms:isFormatOf>https://test.example/1</dcterms:isFormatOf>
              <dcterms:language xsi:type="ISO639-2">eng</dcterms:language>
              <dcterms:language xsi:type="ISO639-2">nld</dcterms:language>
              <dcterms:language>sui</dcterms:language>
              <dcterms:alternative>Alternative title</dcterms:alternative>
              <dcterms:source>source 1</dcterms:source>
              <dcterms:source>source 2</dcterms:source>
              <dcx-dai:contributorDetails>
                  <dcx-dai:author>
                      <dcx-dai:titles>Advocaat</dcx-dai:titles>
                      <dcx-dai:initials>J</dcx-dai:initials>
                      <dcx-dai:insertions></dcx-dai:insertions>
                      <dcx-dai:surname>Harker</dcx-dai:surname>
                      <dcx-dai:role xml:lang="en">Related Person</dcx-dai:role>
                  </dcx-dai:author>
              </dcx-dai:contributorDetails>
              <dcx-dai:contributorDetails>
                  <dcx-dai:organization>
                      <dcx-dai:name xml:lang="en">Advocatenkantoor Harker</dcx-dai:name>
                      <dcx-dai:role xml:lang="en">RightsHolder</dcx-dai:role>
                  </dcx-dai:organization>
              </dcx-dai:contributorDetails>
              <dcterms:hasVersion scheme="DOI">https://doi.org/10.17632/5x6xt4zhws.3</dcterms:hasVersion>
              <dcterms:references href="https://a-v.l.w.com">Anti-Vampire League website</dcterms:references>
              <dcterms:dateAccepted>2016-09-08</dcterms:dateAccepted>
              <dcterms:modified xsi:type="W3CDTF">2017-01-02</dcterms:modified>
              <dcterms:identifier xsi:type="ARCHIS-ZAAK-IDENTIFICATIE">1234567890</dcterms:identifier>
              <dcx-gml:spatial>
                  <Point xmlns="http://www.opengis.net/gml">
                      <pos>
                          52.08113 4.34510
                      </pos>
                  </Point>
              </dcx-gml:spatial>
                  <dcx-gml:spatial>
                      <boundedBy xmlns="http://www.opengis.net/gml">
                          <Envelope srsName="urn:ogc:def:crs:EPSG::28992">
                              <lowerCorner>91232.015554 436172.485680</lowerCorner>
                              <upperCorner>121811.885272 486890.494251</upperCorner>
                          </Envelope>
                      </boundedBy>
                  </dcx-gml:spatial>
          </ddm:dcmiMetadata>
      </ddm:DDM>

    val metadataBlocks = mapper.toDataverseDataset(ddm).get.datasetVersion.metadataBlocks
    val citationJson = Extraction.decompose(metadataBlocks("citation"))
    debug(Serialization.writePretty(citationJson))

    (citationJson \ "displayName") shouldBe JString("Citation Metadata")

    /*
     * Expected:
     * {
     *    "typeName":"title",
     *    "multiple":false,
     *    "typeClass":"primitive",
     *    "value":"A title"
     * }
     */
    val titles = (citationJson \ "fields").filter(_ \ "typeName" == JString("title"))
    titles should have length (1)
    val title = titles.head
    title \ "multiple" shouldBe JBool(false)
    title \ "typeClass" shouldBe JString("primitive")
    title \ "value" shouldBe JString("A title")

    /*
     * Expected:
     * {
     *    "typeName":"otherId",
     *    "multiple":true,
     *    "typeClass":"compound",
     *    "value":[
     *        {
     *          "otherIdValue":{
     *              "typeName":"otherIdValue",
     *              "multiple":false,
     *              "typeClass":"primitive",
     *              "value":"https://test.example/1"
     *           }
     *        }
     *     ]
     * }
     */
    val otherIds = ((citationJson \ "fields")).filter(_ \ "typeName" == JString("otherId"))
    otherIds should have length (1)
    val otherId = otherIds.head
    otherId \ "multiple" shouldBe JBool(true)
    otherId \ "typeClass" shouldBe JString("compound")
    val otherIdValue = (otherId \ "value")
    otherIdValue shouldBe a[JArray]
    otherIdValue.asInstanceOf[JArray].values should have length(1)
  }
}