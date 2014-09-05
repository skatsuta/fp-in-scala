import org.specs2.mutable.Specification

/**
 * Created by a13462 on 8/29/14.
 */
class MyModuleSpec extends Specification {
  "The number -42" should {
    "have the abs value 42" in {
      MyModule.abs(-42) must beEqualTo(42)
    }
  }
  "The fib_normal seq" should {
    "have 0 1 1 2 3 5"  in {
      MyModule.fib_normal(0) must_== 0
      MyModule.fib_normal(1) must_== 1
      MyModule.fib_normal(2) must_== 1
      MyModule.fib_normal(3) must_== 2
      MyModule.fib_normal(4) must_== 3
      MyModule.fib_normal(5) must_== 5
    }
  }
  "The fib_tailrec seq" should {
    "have 0 1 1 2 3 5" in {
      MyModule.fib_tailrec(0) must_== 0
      MyModule.fib_tailrec(1) must_== 1
      MyModule.fib_tailrec(2) must_== 1
      MyModule.fib_tailrec(3) must_== 2
      MyModule.fib_tailrec(4) must_== 3
      MyModule.fib_tailrec(5) must_== 5
    }
  }
}
