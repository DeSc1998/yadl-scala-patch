package stdlib

import interpreterdata.*
import scala.collection.mutable.ArrayBuffer
import parser.{Value, StdString, Number, Bool, Dictionary, Array, Function}

/** Generic utility method that converts any Data Object to a String Object if
  * possible (throws an error otherwise) Can be used outside the stdlib if
  * necessary.
  */
def toStringObj(value: Value): Value = {
  value match {
    case x: (Number | Bool)      => StdString(x.toString)
    case x: StdString            => x
    case x: parser.NoneValue     => StdString("none")
    case x: parser.YadlIterator  => StdString("<iterator>")
    case x: Function             => StdString("<function>")
    case x: (Dictionary | Array) => StdString(serializeJSONCompact(x, Seq()))
  }
}

/** Generic utility method that converts any Data Object to a Boolean Object if
  * possible (throws an error otherwise) Can be used outside the stdlib if
  * necessary.
  */
def toBooleanObj(obj: Value): Value = obj match {
  case x: Bool             => x
  case x: Number           => Bool(x.toString != "0" || x.toString != "0.0")
  case x: StdString        => Bool(x.value.nonEmpty)
  case x: parser.NoneValue => Bool(false)
  case x: Dictionary       => Bool(x.entries.nonEmpty)
  case x: Array            => Bool(x.elements.nonEmpty)
  case x: Function         => Bool(true)
  // Note: Possible while true loop here,
  // but if the programmer returns an iterator in the hastNext
  // function, they don't deserve any better.
  // case x: IteratorObj  => toBooleanObj(x.hasNext.function(Seq(x.data)))
}

/** Generic utility method that converts any Data Object to an Iterator Object
  * if possible (throws an error otherwise) Can be used outside the stdlib if
  * necessary.
  */
def toIteratorObj(obj: DataObject): IteratorObj = obj match {
  case x: IteratorObj => x
  case x: ListObj => {
    val next = FunctionObj(
      Seq("d"),
      Seq(),
      None,
      {
        case Seq(DictionaryObj(dict)) => {
          val indexKey = StringObj("index")
          val lstKey = StringObj("list")
          val curInd = dict.get(indexKey) match {
            case None    => throw IllegalArgumentException()
            case Some(x) => x.asInstanceOf[NumberObj].value.toInt
          }
          val lst = dict.get(lstKey) match {
            case None    => throw IllegalArgumentException()
            case Some(x) => x.asInstanceOf[ListObj].value
          }

          if (curInd >= lst.length) {
            throw IndexOutOfBoundsException()
          } else {
            val rv = lst(curInd)
            dict.addOne(indexKey, NumberObj(curInd + 1))
            rv
          }
        }
        case _ => throw IllegalArgumentException()
      }
    )
    val hasnext = FunctionObj(
      Seq("d"),
      Seq(),
      None,
      {
        case Seq(DictionaryObj(dict)) => {
          val indexKey = StringObj("index")
          val lstKey = StringObj("list")
          val curInd = dict.get(indexKey) match {
            case None    => throw IllegalArgumentException()
            case Some(x) => x.asInstanceOf[NumberObj].value.toInt
          }
          val lst = dict.get(lstKey) match {
            case None    => throw IllegalArgumentException()
            case Some(x) => x.asInstanceOf[ListObj].value
          }

          if (curInd < lst.length) {
            TRUE
          } else {
            FALSE
          }
        }
        case _ => throw IllegalArgumentException()
      }
    )

    val d = scala.collection.mutable.HashMap[DataObject, DataObject]()
    d.addOne(StringObj("index"), NumberObj(0))
    d.addOne(StringObj("list"), ListObj(x.value))

    IteratorObj(next, hasnext, DictionaryObj(d))
  }
  case x: DictionaryObj => throw NotImplementedError() //  TODO implement
  case _                => throw IllegalArgumentException()
}

def toNumberObj(obj: Value): Number = obj match {
  case x: Number => x
  case Bool(x)   => if (x == true) parser.YadlInt(1) else parser.YadlInt(0)
  case _         => throw IllegalArgumentException()
}

def toListObj(obj: DataObject): ListObj = obj match {
  case x: ListObj => x
  case it: IteratorObj => {
    var lst = new ArrayBuffer[DataObject]()
    while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
      lst += it.next.function(Seq(it.data))
    }
    ListObj(lst)
  }
  case dict: DictionaryObj => {
    ListObj(
      dict.value.toList
        .map((k, v) => {
          val b = new ArrayBuffer[DataObject]()
          b += k
          b += v
          ListObj(b)
        })
        .to(ArrayBuffer)
    )
  }
  case _ => throw IllegalArgumentException()
}

private def escapeJsonString(str: String): String = {
  // Escape necessary characters for JSON
  str.flatMap {
    case '"'  => "\\\"" // Escape double quotes
    case '\\' => "\\\\" // Escape backslashes
    case '\b' => "\\b" // Escape backspace
    case '\f' => "\\f" // Escape formfeed
    case '\n' => "\\n" // Escape newline
    case '\r' => "\\r" // Escape carriage return
    case '\t' => "\\t" // Escape tab
    case c if c.isControl =>
      "\\u%04x".format(c.toInt) // Escape control characters
    case c => c.toString // Default case, no escaping needed
  }
}

private def serializeJSONCompact(
    obj: Value,
    references: Seq[Any]
): String = {
  obj match {
    case _: parser.NoneValue    => "\"null\""
    case value: (Bool | Number) => value.toString
    case StdString(value)       => s"\"${escapeJsonString(value)}\""
    case Array(items) => {
      if (references.contains(items)) {
        throw IllegalArgumentException(
          "cannot serialize self referencing data structures"
        )
      }

      "[" + (items.map(e =>
        serializeJSONCompact(e, references :+ items)
      ) mkString ", ") + "]"
    }
    case Dictionary(entries) => {
      "{" + (entries.map((k, v) =>
        serializeJSONCompact(
          k,
          references :+ entries
        ) + ": " + serializeJSONCompact(v, references :+ entries)
      ) mkString ", ") + "}"
    }
    case v => throw UnsupportedOperationException(v.toString)
  }
}
