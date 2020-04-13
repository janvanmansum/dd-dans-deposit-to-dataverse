package nl.knaw.dans.easy.s2d

import better.files.File

/**
 *
 * @param dir
 */
case class Deposit(dir: File) {

  override def toString: String = s"Deposit at $dir"
}
