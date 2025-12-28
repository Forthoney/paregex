signature REGEX_PARSER_EXTENSION =
sig
  val escape: (char, 'strm) StringCvt.reader -> (char Basis.t, 'strm) StringCvt.reader
  val postfix: char Basis.t -> (char, 'strm) StringCvt.reader -> (char Basis.t, 'strm) StringCvt.reader
end
