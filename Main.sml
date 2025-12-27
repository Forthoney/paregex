val isMeta = Char.contains "()|*"
fun escape getc strm =
  case getc strm of
    NONE => NONE
  | SOME (c, strm) =>
    if isMeta c then SOME (Regex.Literal c, strm)
    else
      case c of
        #"a" => SOME (Regex.Literal #"\a", strm)
      | #"f" => SOME (Regex.Literal #"\f", strm)
      | #"t" => SOME (Regex.Literal #"\t", strm)
      | #"n" => SOME (Regex.Literal #"\n", strm)
      | #"r" => SOME (Regex.Literal #"\r", strm)
      | #"v" => SOME (Regex.Literal #"\v", strm)
      | _ => NONE
  
structure Re = RegexParserFn (val escape = escape)

val _ =
  case CommandLine.arguments () of
    [pat] =>
    (case StringCvt.scanString Re.scan pat of
      SOME re =>
      ( print ("parsed: " ^ Regex.toString re ^ "\n")
      ; ThompsonNFA.compile re
      ; print "done\n"
      )
    | NONE => raise Fail "unexpected failure")
  | _ => raise Fail "unsupported"
