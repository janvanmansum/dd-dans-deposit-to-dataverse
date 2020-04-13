package nl.knaw.dans.easy.s2d.queue

import scala.util.Try

trait Task {
  def run(): Try[Unit]
}
