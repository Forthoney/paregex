signature REGEX_PARSER_EXTENSION =
sig
  val escape: (char, 'strm) StringCvt.reader -> (char Regex.t, 'strm) StringCvt.reader
  val postfix: char Regex.t -> (char, 'strm) StringCvt.reader -> (char Regex.t, 'strm) StringCvt.reader
end
