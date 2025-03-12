package stdlib

import interpreterdata._
import parser.YadlInt
import parser.YadlFloat

private type HashMap[K, V] = scala.collection.mutable.HashMap[K, V]

class FunctionContext(
    val function: CallMatch => parser.Value,
    val params: Int,
    val varArgs: Boolean = false,
    val optionals: Seq[(String, parser.Value)] = Seq()
)

class CallMatch(
    val params: Seq[parser.Value],
    val optionals: Seq[(String, parser.Value)],
    val varArgs: Seq[parser.Value],
    val context: FunctionContext
)

def matchCall(
    call_args: Seq[parser.Value],
    context: FunctionContext
): CallMatch =
  // TODO: handling of optional arguments
  if (call_args.length >= context.params && context.varArgs) {
    CallMatch(
      call_args.take(context.params),
      Seq(),
      call_args.drop(context.params),
      context
    )
  } else if (call_args.length == context.params) {
    CallMatch(call_args, Seq(), Seq(), context)
  } else if (context.varArgs)
    throw IllegalArgumentException("Not enough arguments")
  else
    throw IllegalArgumentException(
      s"called function needs ${context.params} argument(s) but got ${call_args.size}"
    )

def builtinPrint(call_match: CallMatch): parser.Value =
  val output = call_match.varArgs
    .map(_.toString)
    .mkString(" ")
  println(output)
  parser.NoneValue()

def builtinWrite(call_match: CallMatch): parser.Value =
  val output = call_match.varArgs
    .map(_.toString)
    .mkString(" ")
  Console.print(output)
  parser.NoneValue()

def asInteger(call_match: CallMatch): parser.Value =
  val value = call_match.params(0)
  if (!value.isInstanceOf[parser.Number])
    throw IllegalArgumentException("passed value was not a number")
  value match {
    case v: YadlInt   => v
    case YadlFloat(v) => YadlInt(v.toLong)
    case _            => assert(false, "unreachble")
  }

/** TODO: we should find a way to add new functions without having to modify
  * this file. But since the interface of the stdlib function wont change we can
  * leave it like this for now. This is THE way to supply prebuild functions to
  * the interpreter.
  * @return
  *   A map of all function names and their corresponding Function Objects.
  */
def builtins: HashMap[String, FunctionContext] = {
  new HashMap[String, FunctionContext]
    .addOne("print", FunctionContext(builtinPrint, 0, true))
    .addOne("write", FunctionContext(builtinWrite, 0, true))
    // ###### type conversion ######
    .addOne(
      "string",
      FunctionContext(
        (call_match) => toStringObj(call_match.params.head),
        1
      )
    )
    .addOne(
      "number",
      FunctionContext(
        (call_match) => toNumberObj(call_match.params.head),
        1
      )
    )
    .addOne("as_int", FunctionContext(asInteger, 1))
    .addOne(
      "bool",
      FunctionContext(
        (call_match) => toBooleanObj(call_match.params.head),
        1
      )
    )
    // ###### stirng utilities ######
    .addOne("split", FunctionContext(stringSplit, 2))
    .addOne("trim", FunctionContext(stringTrim, 1))
    .addOne("repeat", FunctionContext(stringRepeat, 2))
    .addOne("count_substring", FunctionContext(stringCount, 2))
    .addOne("starts_with", FunctionContext(stringStartsWith, 2))
    .addOne("ends_with", FunctionContext(stringEndsWith, 2))
    // ###### processing functions ######
    .addOne("map", FunctionContext(mapBuiltIn, 2))
    .addOne("first", FunctionContext(firstBuiltIn, 3))
    .addOne("last", FunctionContext(lastBuiltIn, 3))
    .addOne("zip", FunctionContext(zipBuiltIn, 2))
    .addOne("group_by", FunctionContext(groupByBuiltin, 2))
    .addOne("flatmap", FunctionContext(flatmapBuiltIn, 2))
    .addOne("filter", FunctionContext(filterBuiltIn, 2))
    .addOne("reduce", FunctionContext(reduceBuiltIn, 2))
    .addOne("sort", FunctionContext(sortBuiltIn, 2))
    .addOne("do", FunctionContext(mapBuiltIn, 2))
    .addOne("count", FunctionContext(countBuiltIn, 2))
    .addOne("check_all", FunctionContext(check_allBuiltIn, 2))
    .addOne("check_any", FunctionContext(check_anyBuiltIn, 2))
    .addOne("check_none", FunctionContext(check_noneBuiltIn, 2))
    .addOne("len", FunctionContext(lenBuiltIn, 1))
    .addOne("flatten", FunctionContext(flattenBuiltIn, 1))
    // ###### IO Operations ######
    .addOne("load", FunctionContext(loadFunction, 2))
    // ###### iterator Operations ######
    .addOne(
      "default_iterator",
      FunctionContext(
        (call_match) => {
          val Seq(list) = call_match.params.take(1)
          iteratorOf(list) match {
            case Some(iter) => iter
            case _ =>
              throw IllegalArgumentException(
                "in default_iterator: argument can not be iterated"
              )
          }
        },
        1
      )
    )
    .addOne("iterator", FunctionContext(iteratorBuiltIn, 3))
    .addOne("has_next", FunctionContext(iteratorHasNext, 1))
    .addOne("next", FunctionContext(iteratorNext, 1))
}
