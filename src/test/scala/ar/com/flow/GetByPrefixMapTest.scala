package ar.com.flow

import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable.HashMap

class GetByPrefix extends HashMap[String, Any] {
  def getForPrefixOf(field: String) = {
    val key = keys.find(key => field.startsWith(key)).getOrElse(field)
    get(key)
  }
}

object GetByPrefix {
  def apply(entries: (String, Any)*): GetByPrefix = {
    val instance = new GetByPrefix()
    entries.foreach({ case (key, value) => instance.put(key, value) })
    instance
  }
}

class GetByPrefixMapTest extends WordSpec with Matchers {
  "GetByPrefix map" when {
    "constructing map with entries" should {
      "contain keys" in {
        val m = GetByPrefix("min_price" -> 1, "sort_position" -> 2)

        m.keys should contain theSameElementsAs Seq("min_price", "sort_position")

      }
      "contain values" in {
        val m = GetByPrefix("min_price" -> 1, "sort_position" -> 2)

        m.values should contain theSameElementsAs Seq(1, 2)
      }
    }
    "retrieving by get" should {
      "return value by key found" in {
        val m = GetByPrefix("min_price" -> 1, "sort_position" -> 2)

        m.get("sort_position") shouldBe Some(2)
      }
      "return None by key not found" in {
        val m = GetByPrefix("min_price" -> 1, "sort_position" -> 2)

        m.get("not_found") shouldBe None
      }
    }
    "retrieving by prefix key" should {
      "return value for prefix key" in {
        val m = GetByPrefix("min_price" -> 1, "sort_position" -> 2)

        m.getForPrefixOf("sort_position_1") shouldBe Some(2)
      }
      "return None by key not found" in {
        val m = GetByPrefix("min_price" -> 1, "sort_position" -> 2)

        m.getForPrefixOf("not_found") shouldBe None
      }
    }
  }
}
