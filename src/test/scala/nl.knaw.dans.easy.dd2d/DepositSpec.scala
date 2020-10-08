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
import org.scalatest.{ FlatSpec, Matchers }

import scala.util.{ Failure, Success }

class DepositSpec extends FlatSpec with Matchers {

  "checkDeposit" should "succeed if directory is deposit" in {
    Deposit(File("src/test/resources/examples/valid-easy-submitted")) shouldBe a[Deposit]
  }

  it should "fail if it is not a directory but a file" in {
    val file = File("src/test/resources/no-deposit/no-dir.txt")
    the [RuntimeException] thrownBy Deposit(file) should have message(s"Not a deposit: $file is not a directory")
  }

  it should "fail if it has no sub-directoires" in {
    val file = File("src/test/resources/no-deposit/no-subdir")
    the [RuntimeException] thrownBy Deposit(file) should have message(s"Not a deposit: $file has more or fewer than one subdirectory")
  }

  it should "fail if it has no deposit.properties" in {
    val file = File("src/test/resources/no-deposit/no-deposit-properties")
    the [RuntimeException] thrownBy Deposit(file) should have message(s"Not a deposit: $file does not contain a deposit.properties file")
  }
}
