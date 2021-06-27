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
package nl.knaw.dans.easy.dd2d.mapping

import better.files.File
import nl.knaw.dans.easy.dd2d.mapping.AbrReportType.toAbrRapportType
import nl.knaw.dans.easy.dd2d.{ Configuration, TestSupportFixture }
import nl.knaw.dans.lib.dataverse.model.dataset.PrimitiveSingleValueField

import java.nio.file.Paths

class AbrReportTypeSpec extends TestSupportFixture with BlockArchaeologySpecific {
  private val reportIdToTerm = Configuration
    .loadCsvToMap(File(Paths.get("src/main/assembly/dist/install/ABR-reports.csv").toAbsolutePath),
      keyColumn = "URI-suffix",
      valueColumn = "Term").get

  "toAbrRapportType" should "return term from list even if different from term in element text" in {
    val reportNumber =
          <ddm:reportNumber
              schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/610a827e-25cd-49df-b220-8499f84ac4e8" subjectScheme="ABR Rapporten" reportNo="S090003">
              Blah blah
          </ddm:reportNumber>
    val abrRapporttypeField = toAbrRapportType(reportIdToTerm)(reportNumber)

    abrRapporttypeField(ABR_RAPPORT_TYPE_TERM).asInstanceOf[PrimitiveSingleValueField].value shouldBe "Synthegra rapport"
  }

  it should "return term from element text if term id is not found in list" in {
    val reportNumber =
          <ddm:reportNumber
              schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/some-new-term-id" subjectScheme="ABR Rapporten" reportNo="S090003">
            A new term not in the static list
          </ddm:reportNumber>
    val abrRapporttypeField = toAbrRapportType(reportIdToTerm)(reportNumber)

    abrRapporttypeField(ABR_RAPPORT_TYPE_TERM).asInstanceOf[PrimitiveSingleValueField].value shouldBe "A new term not in the static list"
  }
}
