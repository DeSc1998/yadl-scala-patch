package stdlib

import parser.Value

def stringSplit(call_match: CallMatch): Value =
  val value = call_match.params(0)
  val split = call_match.params(1)
  if (!value.isInstanceOf[parser.StdString])
    throw IllegalArgumentException("in split: 'value' was not a string")
  if (!split.isInstanceOf[parser.StdString])
    throw IllegalArgumentException("in split: 'split' was not a string")
  val parser.StdString(source) = value.asInstanceOf[parser.StdString]
  val parser.StdString(sep) = split.asInstanceOf[parser.StdString]
  parser.Array(source.split(sep).toSeq.map(parser.StdString.apply))

def stringTrim(call_match: CallMatch): Value =
  val value = call_match.params(0)
  if (!value.isInstanceOf[parser.StdString])
    throw IllegalArgumentException("in trim: 'value' was not a string")
  val parser.StdString(source) = value.asInstanceOf[parser.StdString]
  parser.StdString(source.trim())

def stringRepeat(call_match: CallMatch): Value =
  val value = call_match.params(0)
  val repeats = call_match.params(1)
  if (!value.isInstanceOf[parser.StdString])
    throw IllegalArgumentException("in repeat: 'value' was not a string")
  if (!repeats.isInstanceOf[parser.YadlInt])
    throw IllegalArgumentException("in repeat: 'repeats' was not a int")
  val parser.StdString(source) = value.asInstanceOf[parser.StdString]
  val parser.YadlInt(reps) = repeats.asInstanceOf[parser.YadlInt]
  parser.StdString(source.repeat(reps.toInt))

def stringCount(call_match: CallMatch): Value =
  val value = call_match.params(0)
  val substring = call_match.params(1)
  if (!value.isInstanceOf[parser.StdString])
    throw IllegalArgumentException(
      "in conut_substring: 'value' was not a string"
    )
  if (!substring.isInstanceOf[parser.StdString])
    throw IllegalArgumentException(
      "in count_substring: 'substring' was not a string"
    )
  val parser.StdString(source) = value.asInstanceOf[parser.StdString]
  val parser.StdString(sub) = substring.asInstanceOf[parser.StdString]
  var counts: Int = 0
  if (sub.length == 0) return parser.YadlInt(counts.toLong)
  for (index <- Seq.range(0, source.length - sub.length + 1)) {
    val chars = source.drop(index)
    if (chars.startsWith(sub))
      counts += 1
  }
  parser.YadlInt(counts.toLong)

def stringStartsWith(call_match: CallMatch): Value =
  val value = call_match.params(0)
  val substring = call_match.params(1)
  if (!value.isInstanceOf[parser.StdString])
    throw IllegalArgumentException("in conut: 'value' was not a string")
  if (!substring.isInstanceOf[parser.StdString])
    throw IllegalArgumentException("in count: 'substring' was not a string")
  val parser.StdString(source) = value.asInstanceOf[parser.StdString]
  val parser.StdString(sub) = substring.asInstanceOf[parser.StdString]
  parser.Bool(source.startsWith(sub))

def stringEndsWith(call_match: CallMatch): Value =
  val value = call_match.params(0)
  val substring = call_match.params(1)
  if (!value.isInstanceOf[parser.StdString])
    throw IllegalArgumentException("in conut: 'value' was not a string")
  if (!substring.isInstanceOf[parser.StdString])
    throw IllegalArgumentException("in count: 'substring' was not a string")
  val parser.StdString(source) = value.asInstanceOf[parser.StdString]
  val parser.StdString(sub) = substring.asInstanceOf[parser.StdString]
  parser.Bool(source.endsWith(sub))
