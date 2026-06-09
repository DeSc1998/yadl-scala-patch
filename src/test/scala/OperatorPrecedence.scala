import fastparse._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

import parser.{binaryOpExpression, identifierP, YadlInt, YadlFloat}
import interpreter.{Scope, evalExpression}

def binary[$: P] =
  binaryOpExpression(identifierP, 0)

class OperatorPrecedence extends munit.FunSuite {

  val test_cases: Seq[(String, (Double | Long))] = Seq(
    ("3 + 4 * 5", 23: Long),
    ("(3 + 4) * 5", 35: Long),
    ("3 + (4 * 5) ^ 6", scala.math.pow(20, 6).toLong + 3)
  )

  test_cases.map((input, expected) => {
    test(s"case `$input`") {
      parse(input, binary(using _)) match {
        case Success(result, index) =>
          val resultScope = evalExpression(result, new Scope)
          val Some(res) = resultScope.result: @unchecked
          (res, expected) match {
            case (YadlFloat(r), e: Double) => assertEquals(r, e)
            case (YadlInt(r), e: Long)     => assertEquals(r, e)
            case _ => assert(false, "result types do not match")
          }

          assertEquals(index, input.length, "input has not been parsed fully")
        case _: Failure =>
          assert(false, f"number parsing for test case '$input' failed")
      }
    }
  })

  test("case '3 + 4 * 5 ^ 6'") {
    val input = "3 + 4 * 5 ^ 6"
    val expected = scala.math.pow(5, 6).toLong * 4 + 3
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalExpression(result, new Scope)
        val Some(YadlInt(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '(3 + 4) * 5 ^ 6'") {
    val input = "(3 + 4) * 5 ^ 6"
    val expected = scala.math.pow(5, 6).toLong * 7
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalExpression(result, new Scope)
        val Some(YadlInt(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '(3 + 4 * 5) ^ 6'") {
    val input = "(3 + 4 * 5) ^ 6"
    val expected = scala.math.pow(23, 6).toLong
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalExpression(result, new Scope)
        val Some(YadlInt(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '((3 + 4) * 5) ^ 6'") {
    val input = "((3 + 4) * 5) ^ 6"
    val expected = scala.math.pow(35, 6).toLong
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalExpression(result, new Scope)
        val Some(YadlInt(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '1 - 2 / 3 + 4 * 5 ^ 6'") {
    val input = "1 - 2 / 3 + 4 * 5 ^ 6"
    val expected = 1 - 2.0 / 3.0 + 4 * scala.math.pow(5, 6)
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalExpression(result, new Scope)
        val Some(YadlFloat(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '(1 - 2) / 3 + 4 * 5 ^ 6'") {
    val input = "(1 - 2) / 3 + 4 * 5 ^ 6"
    val expected = (-1 / 3.0) + 4 * scala.math.pow(5, 6)
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalExpression(result, new Scope)
        val Some(YadlFloat(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '1 - 2 / (3 + 4) * 5 ^ 6'") {
    val input = "1 - 2 / (3 + 4) * 5 ^ 6"
    val expected = 1 - 2.0 / (3.0 + 4) * scala.math.pow(5, 6)
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalExpression(result, new Scope)
        val Some(YadlFloat(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '(1 - 2) / (3 + 4) * 5 ^ 6'") {
    val input = "(1 - 2) / (3 + 4) * 5 ^ 6"
    val expected = (1 - 2.0) / (3.0 + 4) * scala.math.pow(5, 6)
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalExpression(result, new Scope)
        val Some(YadlFloat(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '1 - (2 / 3 + 4) * 5 ^ 6'") {
    val input = "1 - (2 / 3 + 4) * 5 ^ 6"
    val expected = 1 - (2.0 / 3.0 + 4) * scala.math.pow(5, 6)
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalExpression(result, new Scope)
        val Some(YadlFloat(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '1 - 2 / 3 + 4 * 5 ^ 6 + 7'") {
    val input = "1 - 2 / 3 + 4 * 5 ^ 6 + 7"
    val expected = 1 - 2.0 / 3.0 + 4 * scala.math.pow(5, 6) + 7
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalExpression(result, new Scope)
        val Some(YadlFloat(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '1 - 2 / (3 + 4) * 5 ^ (6 + 7)'") {
    val input = "1 - 2 / (3 + 4) * 5 ^ (6 + 7)"
    val expected = 1 - 2.0 / (3.0 + 4) * scala.math.pow(5, 6 + 7)
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalExpression(result, new Scope)
        val Some(YadlFloat(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '1 - 2 / 3 + 4 * 5 ^ 6 + 7 * 8'") {
    val input = "1 - 2 / 3 + 4 * 5 ^ 6 + 7 * 8"
    val expected = 1 - 2.0 / 3.0 + 4 * scala.math.pow(5, 6) + 7 * 8
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalExpression(result, new Scope)
        val Some(YadlFloat(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }
}
