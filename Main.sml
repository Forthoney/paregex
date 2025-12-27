val _ =
  case CommandLine.arguments () of
    [pat] =>
    (case StringCvt.scanString Regex.scan pat of
      SOME re =>
      ( print ("parsed: " ^ Regex.toString re ^ "\n")
      ; Thompson.compile re
      ; print "done\n"
      )
    | NONE => raise Fail "unexpected failure")
  | _ => raise Fail "unsupported"
