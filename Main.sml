structure Parser = RegexParserFn (ParserExtension.Standard)

val _ =
  case CommandLine.arguments () of
    [pat] =>
    (case StringCvt.scanString Parser.scan pat of
      SOME re =>
      let
        fun loop 100 = ()
          | loop i = (ThompsonNFA.compile re; loop (i + 1))

        val _ = print "warmup start\n"
        val warmup =
          let
            fun loop start =
              if Time.> (Time.- (Time.now (), start), Time.fromSeconds 3) then ()
              else (ThompsonNFA.compile re; loop start)
          in
            loop (Time.now ())
          end
        val _ = print "warmup end\n"

        val start = Time.now ()
        val _ = loop 0
        val fin = Time.now ()
        val duration = Time.toMicroseconds (Time.- (fin, start))
      in
        print (LargeInt.toString duration ^ " microsecs\n")
      end
    | NONE => raise Fail "unexpected failure")
  | _ => raise Fail "unsupported"
