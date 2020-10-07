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

import nl.knaw.dans.easy.dd2d.dataverse.json.{ ValueObject, createCvFieldSingleValue, createPrimitiveFieldSingleValue }
import nl.knaw.dans.easy.dd2d.mapping.SpatialBox.{ LATLON_SCHEME, RD_SCHEME, getPoint, isRd }

import scala.xml.Node

object SpatialPoint extends Spatial {
  def toEasyTsmSpatialPointValueObject(point: Node): ValueObject = {
    val isRD = isRd(point)// TODO: improve error handling
    val p = getPoint(point)

    Map(
      "scheme" -> createCvFieldSingleValue("easy-tsm-spatial-point",
        if (isRD) RD_SCHEME
        else LATLON_SCHEME),
      "easy-tsm-x" -> createPrimitiveFieldSingleValue("easy-tsm-x", p.x.toString),
      "easy-tsm-y" -> createPrimitiveFieldSingleValue("easy-tsm-y", p.y.toString),
    )
  }
}
