package nl.knaw.dans.easy.s2d.queue

import org.scalatest.{ FlatSpec, Matchers, OneInstancePerTest }

class ActiveTaskQueueSpec extends FlatSpec with Matchers with OneInstancePerTest {


  // TODO: how to make this test robust?
  "start" should "cause previously queued items to be processed (within a reasonable time)" in {
    val q = new ActiveTaskQueue()
    val triggeredTasks = List(
      TriggerTask(),
      TriggerTask(),
      TriggerTask()
    )
    val nonTriggerdTasks = List(
      TriggerTask(),
      TriggerTask(),
      TriggerTask()
    )
    triggeredTasks.foreach(q.add)
    q.start()
    Thread.sleep(5000)
    q.stop()
    triggeredTasks(0).triggered shouldBe true
    triggeredTasks(1).triggered shouldBe true
    triggeredTasks(2).triggered shouldBe true
  }


}
