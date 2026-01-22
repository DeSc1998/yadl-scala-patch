package stdlib

import scala.io.Source
import java.nio.file.Paths
import java.io.{File, PrintWriter}
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.collection.immutable

import interpreterdata.*
import jsoniterator.JsonIterator
import fastparse._, NoWhitespace._
import parser.{
  identifierP,
  Identifier,
  Expression,
  valueP,
  csvP,
  Dictionary,
  Array,
  StdString,
  Value,
  NoneValue,
  YadlFloat,
  YadlInt,
  Bool,
  CsvEntry
}
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import org.typelevel.jawn.FailureException

private def loadFunction(call_match: CallMatch): Value = {
  val Seq(p, f) = call_match.params.take(2)
  if (!p.isInstanceOf[StdString] || !f.isInstanceOf[StdString]) {
    throw IllegalArgumentException(
      "Load function expects two string arguments: path and format"
    )
  }

  val path = p.asInstanceOf[StdString].value
  val format = f.asInstanceOf[StdString].value

  val currentDir = System.getProperty("user.dir")
  val fullPath = Paths.get(currentDir, path).toString
  val source = Source.fromFile(fullPath)

  format match {
    case "json" =>
      val jsonIterator = new JsonIterator(source)
      processJsonIterator(jsonIterator)
    case "lines" =>
      val iter: Iterator[Value] =
        source
          .LineIterator()
          .asInstanceOf[Iterator[String]]
          .map(StdString.apply)
      Array(iter.toSeq)
    case "chars" =>
      StdString(source.mkString)

    case "csv" =>
      parse(source.mkString, csvP(using _)) match {
        case Success(value, _) => csvToDict(value)
        case _: Failure        => scala.sys.error("failed to parse csv")
      }

    case _ => throw IllegalArgumentException(s"Unsupported format: $format")
  }
}

def save(call_match: CallMatch): Value = {
  val Seq(value, p, f) = call_match.params.take(3): @unchecked
  if (!p.isInstanceOf[StdString] || !f.isInstanceOf[StdString]) {
    throw IllegalArgumentException(
      "Load function expects two string arguments: path and format"
    )
  }
  val path = p.asInstanceOf[StdString]
  val format = f.asInstanceOf[StdString]
  var writer = PrintWriter(File(path.value))
  format.value match {
    case "json" => {
      writer.write(s"$value")
    }
    case "csv" => {
      value match {
        case dict: Dictionary => {
          val keys = dict.entries.keys
          writer.write(keys.mkString(","))
          writer.write("\n")
          writer.write(dict.entries.values.mkString(","))
          writer.write("\n")
        }
        case Array(elements) => {
          assert(!elements.isEmpty) // TODO: handle this case
          val none_dict = elements.foldLeft(true)((acc, item) =>
            acc && !item.isInstanceOf[Dictionary]
          )
          val all_dict = elements.foldLeft(true)((acc, item) =>
            acc && item.isInstanceOf[Dictionary]
          )
          if (none_dict || !all_dict) {
            writer.write(elements.mkString("\n"))
            writer.write("\n")
          } else {
            // NOTE: we assume all Dictionaries have the same keys
            val first = elements.head.asInstanceOf[Dictionary]
            val keys = first.entries.keys
            writer.write(keys.mkString(","))
            writer.write("\n")
            elements.map((item) => {
              val dict = item.asInstanceOf[Dictionary]
              val out = keys.map({
                dict.entries.getOrElse(_, Some(StdString("")))
              })
              writer.write(out.mkString(","))
              writer.write("\n")
            })
          }
        }
        case v => assert(false, s"saving as CSV: Not implemented for $v")
      }
    }
    case format =>
      assert(false, s"saving as CSV: Not supported for format '$format'")
  }
  writer.close
  NoneValue()
}

def csvToDict(csv: parser.CSV): Value = csv.header match {
  case Some(header) => {
    val h = header.map(StdString.apply)
    var entries: Seq[Value] = Seq()
    for (row <- csv.data) {
      var map = HashMap[Value, Value]()
      for ((key, value) <- h.zip(row.map(_.asInstanceOf[Value])))
        map.put(key, value)
      entries = entries :+ Dictionary(map)
    }
    Array(entries)
  }
  case None =>
    Array(
      csv.data.map((row) => Array(row.map(_.asInstanceOf[Value])))
    )
}

def valueParser[$: P]: P[Value] = P(
  valueP(identifierP)
    .filter(!_.isInstanceOf[Value])
    .map(_.asInstanceOf[Value])
)

private def parseHeader(lineIter: Iterator[String]): Option[Seq[Expression]] = {
  val iter = lineIter.filter(line =>
    line
      .split(",")
      .forall(elem =>
        elem.forall(char => Character.isAlphabetic(char) || char == '_') || elem
          .length() > 0 && elem(0) == '"' && elem(
          elem.length() - 1
        ) == '"'
      )
  )
  if (iter.hasNext) {
    val header = iter
      .map { line =>
        line
          .split(",")
          .map((v: String) =>
            parse(v, valueParser(using _)) match {
              case Parsed.Success(Identifier(name), _) => StdString(name)
              case Parsed.Success(v, _)                => v
              case e: Parsed.Failure =>
                scala.sys.error("failed to parse a value in a csv file")
            }
          )
      }
      .take(1)

    Some(header.next().toSeq)
  } else None
}

private def processJsonIterator(iterator: JsonIterator): Value = {
  val result = HashMap[String, Value]()

  while (iterator.hasNext) {
    iterator.next() match {
      case Right((key, value)) =>
        val valueObj = convertJsonExpression(value)
        result(key) = valueObj
      case Left(error) =>
        throw IllegalArgumentException(s"Error reading from iterator: $error")
    }
  }

  convertToProperStructure(result)
}

private def convertToProperStructure(
    map: HashMap[String, Value]
): Value = {
  if (map.keys.forall(_.matches("""\[\d+\]"""))) {
    val sortedEntries = map.toSeq
      .sortBy { case (key, _) =>
        key.substring(1, key.length - 1).toInt
      }
      .map(_._2)
    Array(sortedEntries)
  } else {
    Dictionary(map.map { case (k, v) => StdString(k) -> v })
  }
}

private def convertJsonExpression(value: Any): Value = {
  value match {
    case null | None => NoneValue()
    case b: Boolean  => Bool(b)
    case n: BigDecimal =>
      if (n.isValidInt)
        YadlInt(n.toLong)
      else
        YadlFloat(n.toDouble)
    case s: String        => StdString(s)
    case it: JsonIterator => processJsonIterator(it)
    case other =>
      throw IllegalArgumentException(
        s"Unsupported JSON value type: ${other.getClass}"
      )
  }
}
