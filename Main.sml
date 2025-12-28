structure Parser = RegexParserFn (ParserExtension.Simple)

val _ =
  case CommandLine.arguments () of
    [pat] =>
    (case StringCvt.scanString Parser.scan pat of
      SOME re =>
      ( print ("parsed: " ^ Regex.toString re ^ "\n")
      ; ThompsonNFA.compile re
      ; print "done\n"
      )
    | NONE => raise Fail "unexpected failure")
  | _ => raise Fail "unsupported"
