package functional_data_structures

import org.specs2.mutable.Specification

class MyListSpec extends Specification {
  "tail" should {
    "return a list except the first element" in {
      MyList.tail(List(1, 2, 3)) must_== List(2, 3)
    }

    "return Nil when applied to Nil" in {
      MyList.tail(Nil) must_== Nil
    }
  }

  "setHead" should {
    "return lists that have different values in its head when applied to different values" in {
      MyList.setHead(Nil, 1) must_== Nil
      MyList.setHead(List(1), 2) must_== List(2)
      MyList.setHead(List("c", "b"), "a") must_== List("a", "b")
    }
  }

  "drop" should {
    "remove the first n elements from a list" in {
      MyList.drop(List(1), -1) must_== List(1)
      MyList.drop(List(1), 0) must_== List(1)
      MyList.drop(Nil, 3) must_== Nil

      MyList.drop(List(1), 1) must_== Nil
      MyList.drop(List(1), 2) must_== Nil
      MyList.drop(List(1, 2), 1) must_== List(2)
      MyList.drop(List(1, 2, 3), 2) must_== List(3)
    }
  }
}
