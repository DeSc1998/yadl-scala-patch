package stdlib

import interpreterdata.*
import parser.{Value, Array, Dictionary, YadlInt}

import scala.collection.mutable
import scala.util.boundary, boundary.break
import parser.YadlIterator
import parser.NoneValue
import parser.Bool

private def filterBuiltIn(params: Seq[DataObject]): DataObject = {
  if (params.length != 2 || !params(1).isInstanceOf[FunctionObj]) {
    throw IllegalArgumentException()
  }

  val filterFn = params(1).asInstanceOf[FunctionObj]

  // We can cheat and just don't use the the data attribute of the
  // iterator, since it can't be accessed by the language in the
  // first place.
  val d = DictionaryObj(mutable.HashMap[DataObject, DataObject]())

  val it = toIteratorObj(params(0))
  var buffer: DataObject = NONE
  var hasBuffer = false

  val hasnext = FunctionObj(
    Seq("d"),
    Seq(),
    None,
    (d: Seq[DataObject]) => {
      if (hasBuffer) {
        TRUE
      }

      boundary {
        while (
          it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value
        ) {
          val n = it.next.function(Seq(it.data))

          if (filterFn.function(Seq(n)).asInstanceOf[BooleanObj].value) {
            buffer = n
            hasBuffer = true
            break(TRUE)
          }
        }
        FALSE
      }
    }
  )

  val next = FunctionObj(
    Seq("d"),
    Seq(),
    None,
    (d: Seq[DataObject]) => {
      if (hasBuffer) {
        hasBuffer = false
        buffer
      } else if (hasnext.function(d).asInstanceOf[BooleanObj].value) {
        hasBuffer = false
        buffer
      } else {
        throw IllegalArgumentException()
      }
    }
  )
  IteratorObj(next, hasnext, d)
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
    case v => throw NotImplementedError(v.toString())
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
    "in check_all: second argument is not a function"
  )
  Bool(
    !check_helper(items, fn.asInstanceOf[parser.Function], false, { _ || _ })
  )
}

private def firstBuiltIn(params: Seq[DataObject]): DataObject = {
  if (
    params.length < 1 || params.length > 3 || !params(1)
      .isInstanceOf[FunctionObj]
  ) {
    throw new IllegalArgumentException
  }

  val it = toIteratorObj(params(0)).asInstanceOf[IteratorObj]
  val fn =
    if (params.length > 1 && params(1).isInstanceOf[FunctionObj])
      Some(params(1).asInstanceOf[FunctionObj])
    else None
  val default = if (params.length == 3) Some(params(2)) else None

  while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
    val nextElement = it.next.function(Seq(it.data))
    if (
      fn.isEmpty || fn.get
        .function(Seq(nextElement))
        .asInstanceOf[BooleanObj]
        .value
    ) {
      return nextElement
    }
  }

  default.getOrElse(
    throw new NoSuchElementException("No element satisfies the condition")
  )
}

private def lastBuiltIn(params: Seq[DataObject]): DataObject = {
  if (
    params.length < 1 || params.length > 3 || !params(1)
      .isInstanceOf[FunctionObj]
  ) {
    throw new IllegalArgumentException
  }

  val it = toIteratorObj(params(0)).asInstanceOf[IteratorObj]
  val fn =
    if (params.length > 1 && params(1).isInstanceOf[FunctionObj])
      Some(params(1).asInstanceOf[FunctionObj])
    else None
  val default = if (params.length == 3) Some(params(2)) else None

  var lastElement: DataObject = default.getOrElse(UndefinedObj())
  var found = false

  while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
    val nextElement = it.next.function(Seq(it.data))
    if (
      fn.isEmpty || fn.get
        .function(Seq(nextElement))
        .asInstanceOf[BooleanObj]
        .value
    ) {
      lastElement = nextElement
      found = true
    }
  }

  if (found) {
    lastElement
  } else {
    default.getOrElse(
      throw new NoSuchElementException("No element satisfies the condition")
    )
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
    case v => throw NotImplementedError(v.toString())
  }
}

private def zipBuiltIn(params: Seq[DataObject]): IteratorObj = {
  // Check if there are exactly two parameters and both are iterators
  if (params.length != 2) {
    throw new IllegalArgumentException(
      "zipBuiltIn expects exactly two parameters"
    )
  }

  // Extract the iterators
  val it1 = toIteratorObj(params(0)).asInstanceOf[IteratorObj]
  val it2 = toIteratorObj(params(1)).asInstanceOf[IteratorObj]

  // Initialize an empty ArrayBuffer to store the zipped results
  val zippedResults = mutable.ArrayBuffer.empty[mutable.ArrayBuffer[DataObject]]

  // Iterate until either iterator is empty
  while (
    it1.hasNext.function(Seq(it1.data)).asInstanceOf[BooleanObj].value &&
    it2.hasNext.function(Seq(it2.data)).asInstanceOf[BooleanObj].value
  ) {
    // Collect elements from each iterator into an ArrayBuffer
    val tuple = mutable.ArrayBuffer(
      it1.next.function(Seq(it1.data)),
      it2.next.function(Seq(it2.data))
    )
    zippedResults += tuple
  }

  // Convert zippedResults to an iterator
  val newIt = zippedResults.iterator

  // Functions to wrap the new iterator
  val hasNextFn =
    new FunctionObj(Seq(), Seq(), None, _ => BooleanObj(newIt.hasNext))
  val nextFn = new FunctionObj(
    Seq(),
    Seq(),
    None,
    _ => {
      if (newIt.hasNext) {
        val nextTuple = newIt.next()
        ListObj(
          nextTuple
        ) // Wrap nextTuple in ListObj (which expects ArrayBuffer)
      } else {
        NoneObj()
      }
    }
  )
  val data = DictionaryObj(mutable.HashMap())

  // Return the new IteratorObj
  new IteratorObj(nextFn, hasNextFn, data)
}

private def sortBuiltIn(call_match: CallMatch): Value = {
  val Seq(items, compare: parser.Function) =
    call_match.params.take(2): @unchecked
  items match {
    case Array(xs) =>
      Array(xs.sortWith((x, y) => {
        var scope = interpreter.Scope()
        val Some(result) = interpreter
          .evalFunctionCall(
            compare,
            Seq(x, y),
            scope,
            interpreter.CallContext.Expression
          )
          .result: @unchecked
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

private def groupByBuiltIn(params: Seq[DataObject]): DataObject = {
  if (params.length != 2 || !params(1).isInstanceOf[FunctionObj]) {
    throw IllegalArgumentException()
  }

  val groupByFn = params(1).asInstanceOf[FunctionObj]
  val it = toIteratorObj(params(0))
  val result = new DictionaryObj(
    scala.collection.mutable.HashMap[DataObject, DataObject]()
  )

  while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
    val item = it.next.function(Seq(it.data))
    val key = groupByFn.function(Seq(item))

    if (!result.value.contains(key)) {
      result.value(key) = new ListObj(
        scala.collection.mutable.ArrayBuffer[DataObject]()
      )
    }
    result.value(key).asInstanceOf[ListObj].value.append(item)
  }

  result
}

private def reduceBuiltIn(call_match: CallMatch): Value = {
  val Seq(items, callable: parser.Function) =
    call_match.params.take(2): @unchecked

  items match {
    case Array(xs) =>
      xs.drop(1)
        .foldLeft(xs(0))((acc, x) => applyRuntime(callable, Seq(acc, x)))
  }
}

private def mapBuiltIn(call_match: CallMatch): Value = {
  val Seq(items, fn: parser.Function) = call_match.params.take(2): @unchecked

  items match {
    case Array(elems) =>
      Array(elems.map((x: Value) => applyRuntime(fn, Seq(x))))
    case iterator: YadlIterator =>
      val nextFn = (data: Seq[Value]) => {
        val iter: YadlIterator = data(0).asInstanceOf[YadlIterator]
        val (value: Value, new_data: Seq[Value]) =
          iter.next_fn(iter.data): @unchecked
        val mapped: Value = applyRuntime(fn, Seq(value))
        (mapped, new_data)
      }

      YadlIterator(nextFn, iterator.has_next_fn, None, Seq(iterator))
    case _: Value =>
      throw IllegalArgumentException("in map: provided value is not a sequence")
  }
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
  val Seq(next_fn: parser.Function, has_next_fn: parser.Function, data: Value) =
    call_match.params.take(3): @unchecked
  val iter_next: Seq[Value] => (Value, Seq[Value]) = (data) =>
    val value = applyRuntime(next_fn, data)
    (value, data)

  val iter_has_next: Seq[Value] => Boolean = (data) =>
    val Bool(value) = applyRuntime(has_next_fn, data): @unchecked
    value

  YadlIterator(iter_next, iter_has_next, None, Seq(data))
}

private def iteratorHasNext(call_match: CallMatch): Value = {
  val Seq(iter: YadlIterator) = call_match.params.take(1): @unchecked
  Bool(iter.has_next_fn(iter.data))
}

private def iteratorNext(call_match: CallMatch): Value = {
  var Seq(iter: YadlIterator) = call_match.params.take(1): @unchecked
  val (value, data) = iter.next_fn(iter.data)
  iter.data = data
  value
}

def iteratorOf(value: Value): Option[YadlIterator] =
  value match {
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
    case _: Value => None
  }
