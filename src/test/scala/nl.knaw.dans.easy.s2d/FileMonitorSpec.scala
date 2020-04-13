package nl.knaw.dans.easy.s2d

import java.util.concurrent.Executors

import better.files.{ File, FileMonitor }
import org.scalatest.{ FlatSpec, Matchers, OneInstancePerTest }

import scala.concurrent.ExecutionContext

//class FileMonitorSpec extends FlatSpec with Matchers with OneInstancePerTest {
//  implicit val ec: ExecutionContext = Executors.newSingleThreadExecutor()
//
//  "start" should "start a new thread" in {
//
//    val m = new FileMonitor(File("/Users/janm/Downloads"), 3) {
//      override def onCreate(file: File, count: Int): Unit = {
//        println(s"Created $file")
//      }
//    }
//
//    m.start()
//    println("stop")
//  }
//
//}
