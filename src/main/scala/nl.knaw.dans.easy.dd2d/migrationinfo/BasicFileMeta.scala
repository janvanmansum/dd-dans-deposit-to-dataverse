package nl.knaw.dans.easy.dd2d.migrationinfo

import nl.knaw.dans.lib.dataverse.model.file.prestaged.DataFile

import java.nio.file.Path

case class BasicFileMeta(datasetSequenceNumber: Int,
                         label: Option[String],
                         directoryLabel: Option[String],
                         dataFile: DataFile)
