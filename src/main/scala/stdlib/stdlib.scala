package stdlib

import interpreterdata._
import scala.annotation.meta.param

private type HashMap[K, V] = scala.collection.mutable.HashMap[K, V]

class FunctionContext(
    val params: Int,
    val optionals: Seq[(String, parser.Value)],
    val varArgs: Boolean,
    val function: CallMatch => parser.Value
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
  } else throw IllegalArgumentException("Not enough arguments")

def builtinPrint(call_match: CallMatch): parser.Value =
  val output = call_match.params
    .map(_.toString)
    .mkString(" ")
  println(output)
  parser.NoneValue()

def builtinWrite(call_match: CallMatch): parser.Value =
  val output = call_match.params
    .map(_.toString)
    .mkString(" ")
  print(output)
  parser.NoneValue()

/** TODO: we should find a way to add new functions without having to modify
  * this file. But since the interface of the stdlib function wont change we can
  * leave it like this for now. This is THE way to supply prebuild functions to
  * the interpreter.
  * @return
  *   A map of all function names and their corresponding Function Objects.
  */
def builtins: HashMap[String, FunctionContext] = {
  new HashMap[String, FunctionContext]
    .addOne("print", FunctionContext(0, Seq(), true, builtinPrint))
    .addOne("write", FunctionContext(0, Seq(), true, builtinWrite))
    .addOne(
      "string",
      FunctionContext(
        1,
        Seq(),
        false,
        (call_match) => toStringObj(call_match.params.head)
      )
    )
    .addOne(
      "number",
      FunctionContext(
        1,
        Seq(),
        false,
        (call_match) => toNumberObj(call_match.params.head)
      )
    )
    .addOne(
      "bool",
      FunctionContext(
        1,
        Seq(),
        false,
        (call_match) => toBooleanObj(call_match.params.head)
      )
    )
}
