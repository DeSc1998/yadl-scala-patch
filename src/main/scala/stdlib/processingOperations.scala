package stdlib

import interpreterdata.*
import parser.{Value, Array, Dictionary, YadlInt}

import scala.collection.mutable
import scala.util.boundary, boundary.break
import parser.YadlIterator
import parser.NoneValue
import parser.Bool
import java.rmi.UnexpectedException

private def groupByBuiltin(call_match: CallMatch): Value = {
  val Seq(items, callable) = call_match.params.take(2)
  assert(
    callable.isInstanceOf[parser.Function],
    "in group_by: second argument is not a function"
  )
  val groupper = callable.asInstanceOf[parser.Function]
  items match {
    case Array(xs) =>
      val uniques = xs.map((x) => applyRuntime(groupper, Seq(x))).distinct
      val entries =
        uniques
          .map((x) => {
            (
              x,
              Array(xs.filter((y) => {
                val r = applyRuntime(groupper, Seq(y))
                var s = interpreter.Scope()
                val Some(result) = interpreter
                  .evalCompareOps(parser.CompareOps.Eq, r, x, s)
                  .result: @unchecked
                result.asInstanceOf[parser.Bool].b
              }))
            )
          })
      var out = mutable.HashMap[Value, Value]()
      parser.Dictionary(out.addAll(entries))
    case v => throw NotImplementedError(v.getClass.getName())
  }
}

private def check_helper(
    items: Value,
    fn: parser.Function,
    initial: Boolean,
    bin_op: (Boolean, Boolean) => Boolean
): Boolean =
  items match {
    case Array(xs) =>
      xs
        .map((x) => applyRuntime(fn, Seq(x)))
        .foldLeft(initial)((acc, x) =>
          x match {
            case Bool(b) => bin_op(b, acc)
            case _       => acc
          }
        )
    case v => throw NotImplementedError(v.getClass.getName())
  }

private def check_allBuiltIn(call_match: CallMatch): Value = {
  val Seq(items, fn) = call_match.params.take(2)
  assert(
    fn.isInstanceOf[parser.Function],
    "in check_all: second argument is not a function"
  )
  Bool(check_helper(items, fn.asInstanceOf[parser.Function], true, { _ && _ }))
}

private def check_anyBuiltIn(call_match: CallMatch): Value = {
  val Seq(items, fn) = call_match.params.take(2)
  assert(
    fn.isInstanceOf[parser.Function],
    "in check_all: second argument is not a function"
  )
  Bool(check_helper(items, fn.asInstanceOf[parser.Function], false, { _ || _ }))
}

private def check_noneBuiltIn(call_match: CallMatch): Value = {
  val Seq(items, fn) = call_match.params.take(2)
  assert(
    fn.isInstanceOf[parser.Function],
    "in check_none: second argument is not a function"
  )
  Bool(
    !check_helper(items, fn.asInstanceOf[parser.Function], false, { _ || _ })
  )
}

private def firstBuiltIn(call_match: CallMatch): Value = {
  val Seq(items, callable, default) = call_match.params.take(3)
  assert(
    callable.isInstanceOf[parser.Function],
    "in first: second argument is not a function"
  )
  val fn = callable.asInstanceOf[parser.Function]
  items match {
    case Array(xs) =>
      xs.find((x) =>
        val result = applyRuntime(fn, Seq(x))
        firstIfTrue(result, true, false)
      ) match {
        case Some(v) => v
        case None    => default
      }
    case v => throw NotImplementedError(v.getClass.getName())
  }
}

private def lastBuiltIn(call_match: CallMatch): Value = {
  val Seq(items, callable, default) = call_match.params.take(3)
  assert(
    callable.isInstanceOf[parser.Function],
    "in last: second argument is not a function"
  )
  val fn = callable.asInstanceOf[parser.Function]
  items match {
    case Array(xs) =>
      xs.findLast((x) =>
        val result = applyRuntime(fn, Seq(x))
        firstIfTrue(result, true, false)
      ) match {
        case Some(v) => v
        case None    => default
      }
    case v => throw NotImplementedError(v.getClass.getName())
  }
}

private def firstIfTrue[T](condition: Value, on_true: T, on_false: T): T =
  condition match {
    case Bool(true) => on_true
    case _          => on_false
  }

private def countBuiltIn(call_match: CallMatch): Value = {
  val Seq(items, callable) = call_match.params.take(2)
  assert(
    callable.isInstanceOf[parser.Function],
    "second argument is not a function"
  )
  val predicate = callable.asInstanceOf[parser.Function]
  items match {
    case Array(xs) =>
      val bools = xs.map((x) => applyRuntime(predicate, Seq(x)))
      YadlInt(
        bools.foldLeft(0)((acc, x) => firstIfTrue(x, acc + 1, acc))
      )
    case Dictionary(entries) =>
      val count = entries.values
        .map((x) => applyRuntime(predicate, Seq(x)))
        .foldLeft(0)((acc, x) => firstIfTrue(x, acc + 1, acc))
      YadlInt(count)
    case v => throw NotImplementedError(v.getClass.getName())
  }
}

private def zipBuiltIn(call_match: CallMatch): Value = {
  val Seq(items_left, items_right) = call_match.params.take(2)
  (items_left, items_right) match {
    case (Array(xs), Array(ys)) =>
      val min_size = scala.math.min(xs.size, ys.size)
      Array(
        xs.take(min_size)
          .zip(ys.take(min_size))
          .map((p: (Value, Value)) => {
            val (x, y) = p
            Array(Seq(x, y))
          })
      )
    case (v1, v2) =>
      throw NotImplementedError(
        v1.getClass.getName() + " and " + v2.getClass.getName()
      )
  }
}

private def sortBuiltIn(call_match: CallMatch): Value = {
  val Seq(items, callable) = call_match.params.take(2)
  assert(
    callable.isInstanceOf[parser.Function],
    "in sort: second argument is not a function"
  )
  val compare = callable.asInstanceOf[parser.Function]
  items match {
    case Array(xs) =>
      Array(xs.sortWith((x, y) => {
        val result = applyRuntime(compare, Seq(x, y))
        result match {
          case Bool(b) => b
          case _ =>
            throw IllegalArgumentException(
              "compare function did not return boolean"
            )
        }
      }))
    case v => throw NotImplementedError(v.toString())
  }
}

private def lenBuiltIn(call_match: CallMatch): Value = {
  val Seq(items) = call_match.params.take(1)
  items match {
    case Array(elements) => YadlInt(elements.size.toLong)
    case _: Value        => YadlInt(0)
  }
}

private def reduceBuiltIn(call_match: CallMatch): Value = {
  val Seq(items, callable: parser.Function) =
    call_match.params.take(2): @unchecked

  items match {
    case Array(xs) =>
      xs.drop(1)
        .foldLeft(xs(0))((acc, x) => applyRuntime(callable, Seq(acc, x)))

    case iter: YadlIterator => {
      var acc: Value = NoneValue()
      var cond = firstIfTrue(iter_has_next(iter), true, false)
      while (cond) {
        val value = iter_next(iter)
        if (acc.isInstanceOf[NoneValue])
          acc = value
        else
          acc = applyRuntime(callable, Seq(acc, value))
        cond = firstIfTrue(iter_has_next(iter), true, false)
      }
      acc
    }
  }
}

private def flattenBuiltIn(call_match: CallMatch): Value = {
  var Seq(items) = call_match.params.take(1)
  items match {
    case Array(xs) =>
      Array(
        xs.foldLeft(Seq())((acc, x) =>
          x match {
            case Array(values) => acc :++ values
            case v             => acc :+ v
          }
        )
      )

    case v => throw NotImplementedError(v.getClass.getName())
  }
}

private def filterBuiltIn(call_match: CallMatch): Value = {
  val Seq(items, callable) = call_match.params.take(2)
  assert(
    callable.isInstanceOf[parser.Function],
    "in filter: second argument is not a function"
  )
  val fn = callable.asInstanceOf[parser.Function]
  items match {
    case Array(xs) =>
      Array(xs.filter((x) => {
        val result = applyRuntime(fn, Seq(x))
        result match {
          case Bool(b) => b
          case v =>
            throw UnexpectedException(
              "in filter: expected result of type bool but got " + v.getClass
                .getName()
            )
        }
      }))
    case v => throw NotImplementedError(v.getClass.getName())
  }
}

private def mapBuiltIn(call_match: CallMatch): Value = {
  val Seq(items, callalbe) = call_match.params.take(2)
  assert(
    callalbe.isInstanceOf[parser.Function],
    "in map: second argument is not a function"
  )
  val fn = callalbe.asInstanceOf[parser.Function]

  items match {
    case Array(elems) =>
      Array(elems.map((x: Value) => applyRuntime(fn, Seq(x))))
    case iterator: YadlIterator =>
      val nextFn = (data: Seq[Value]) => {
        val Seq(iter: YadlIterator) = data.take(1): @unchecked
        val (value: Value, new_data: Seq[Value]) =
          iter.next_fn(iter.data): @unchecked
        val mapped: Value = applyRuntime(fn, Seq(value))
        iter.data = new_data
        (mapped, Seq(iter))
      }
      val hasNextFn = (data: Seq[Value]) =>
        val Seq(iter: YadlIterator) = data.take(1): @unchecked
        iter.has_next_fn(iter.data)

      YadlIterator(nextFn, hasNextFn, None, Seq(iterator))
    case _: Value =>
      throw IllegalArgumentException("in map: provided value is not a sequence")
  }
}

private def flatmapBuiltIn(call_match: CallMatch): Value = {
  val res = mapBuiltIn(call_match)
  val _new = call_match.params.updated(0, res)
  flattenBuiltIn(
    CallMatch(
      _new,
      call_match.optionals,
      call_match.varArgs,
      call_match.context
    )
  )
}

def applyRuntime(fn: parser.Function, args: Seq[Value]): Value =
  var scope = interpreter.Scope(interpreter.Scope())
  val evaled = interpreter.evalFunctionCall(
    fn,
    args,
    scope,
    interpreter.CallContext.Expression
  )
  evaled.result match {
    case Some(value: Value) => value
    case _                  => NoneValue()
  }

private def iteratorBuiltIn(call_match: CallMatch): Value = {
  val Seq(next_fn: parser.Function, has_next_fn: parser.Function, data) =
    call_match.params.take(3): @unchecked
  val iter_next = (data: Seq[Value]) =>
    val value = applyRuntime(next_fn, data)
    (value, data)

  val iter_has_next = (data: Seq[Value]) =>
    val result = applyRuntime(has_next_fn, data): @unchecked
    firstIfTrue(result, true, false)

  YadlIterator(iter_next, iter_has_next, None, Seq(data))
}

private def iteratorHasNext(call_match: CallMatch): Value = {
  var Seq(it) = call_match.params.take(1)
  assert(
    it.isInstanceOf[YadlIterator],
    "in next: argument is not an iterator"
  )
  val iter = it.asInstanceOf[YadlIterator]
  iter_has_next(iter)
}

private def iteratorNext(call_match: CallMatch): Value = {
  var Seq(it) = call_match.params.take(1)
  assert(
    it.isInstanceOf[YadlIterator],
    "in next: argument is not an iterator"
  )
  val iter = it.asInstanceOf[YadlIterator]
  iter_next(iter)
}

private def iter_next(iter: YadlIterator): Value =
  val (value, data) = iter.next_fn(iter.data)
  iter.data = data
  value

private def iter_has_next(iter: YadlIterator): Value =
  Bool(iter.has_next_fn(iter.data))

def iteratorOf(value: Value): Option[YadlIterator] =
  value match {
    case i: YadlIterator => Some(i)
    case a: Array =>
      val data = Seq(YadlInt(0), a)
      val hasNextFn = (data: Seq[Value]) => {
        val Seq(index: YadlInt, array: Array) = data: @unchecked
        index.value < array.elements.size
      }
      val nextFn: Seq[Value] => (Value, Seq[Value]) = (data: Seq[Value]) => {
        val Seq(index: YadlInt, array: Array) = data: @unchecked
        if (index.value < array.elements.size) {
          val tmp = array.elements(index.value.toInt)
          (tmp, Seq(YadlInt(index.value + 1), array))
        } else (NoneValue(), data)
      }
      Some(YadlIterator(nextFn, hasNextFn, None, data))
    case d: Dictionary =>
      // TODO: avoid copying of all entries
      val items = Array(
        d.entries
          .map((p: (Value, Value)) =>
            import parser.StdString
            val (x, y) = p
            Dictionary(
              mutable
                .HashMap((StdString("key") -> x), (StdString("value") -> y))
            )
          )
          .toSeq
      )
      val data = Seq(YadlInt(0), items)
      val hasNextFn = (data: Seq[Value]) => {
        val Seq(index: YadlInt, array: Array) = data: @unchecked
        index.value < array.elements.size
      }
      val nextFn: Seq[Value] => (Value, Seq[Value]) = (data: Seq[Value]) => {
        val Seq(index: YadlInt, array: Array) = data: @unchecked
        if (index.value < array.elements.size) {
          val tmp = array.elements(index.value.toInt)
          (tmp, Seq(YadlInt(index.value + 1), array))
        } else (NoneValue(), data)
      }
      Some(YadlIterator(nextFn, hasNextFn, None, data))
    case _: Value => None
  }
