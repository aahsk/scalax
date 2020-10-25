package com.github.t3hnar.scalax.util

import org.specs2.mutable.Specification
import java.util.concurrent.TimeUnit
import org.specs2.specification.Scope
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

/**
 * @author Yaroslav Klymko
 */
class ExpiringCacheSpec extends Specification {
  "ExpiringCache" should {
    "be constructable with FiniteDuration" in {
      implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global;
      val cache = new ExpiringCache[Int, String](FiniteDuration(1, TimeUnit.MILLISECONDS), 5);
      cache.duration shouldEqual 1
      cache.unit shouldEqual TimeUnit.MILLISECONDS
      cache.queryOverflow shouldEqual 5
    }

    "be capable of telling time internally" in {
      implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global;
      class ExposedExpiringCache(
        val durationPrim:      FiniteDuration,
        val queryOverflowPrim: Int)(implicit ec: ExecutionContext) extends ExpiringCache[Int, String](
        durationPrim,
        queryOverflowPrim) {
        def exposedCurrentMillis: Long = currentMillis
      }
      val cache = new ExposedExpiringCache(FiniteDuration(1, TimeUnit.MILLISECONDS), 5)
      cache.exposedCurrentMillis must not(throwA[Exception])
    }

    "clean expired values if get enough queries" in new ExpiringCacheScope {
      cache.map must haveSize(0)
      cache.queryCount mustEqual 0

      cache.put(0, "-2")
      val oldValue: Option[String] = cache.put(0, "-1")
      oldValue must beSome("-2")
      cache.remove(0)
      cache.get(0) must beNone

      cache.put(0, "0")
      cache.get(0) must beSome("0")

      cache.map must haveSize(1)
      cache.queryCount mustEqual 2

      current = cache.unit.toMillis(cache.duration)

      cache.put(1, "1")
      cache.get(1) must beSome("1")
      cache.queryCount mustEqual 3

      (0 to cache.queryOverflow).foreach(_ => cache.get(3))

      cache.map.size must eventually(beEqualTo(1))
      cache.get(1) must beSome("1")
    }

    "not return expired values which are not cleaned" in new ExpiringCacheScope {
      cache.map must haveSize(0)
      cache.queryCount mustEqual 0

      cache.put(0, "0")
      cache.get(0) must beSome("0")
      cache.map.size must eventually(beEqualTo(1))

      current = cache.unit.toMillis(cache.duration)

      cache.get(0) must beNone
      cache.map.size must eventually(beEqualTo(1))
    }

  }

  class ExpiringCacheScope extends Scope {
    var current = 0L
    val cache = new ExpiringCache[Int, String](1, TimeUnit.MILLISECONDS, 5) {
      override def currentMillis = current
    }
  }
}

