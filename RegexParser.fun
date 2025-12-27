functor RegexParserFn
  (val escape: (char, 'strm) StringCvt.reader -> (char Regex.t, 'strm) StringCvt.reader) =
struct
  structure Re = Regex
  fun scan getc strm =
    let
      exception Scan of string
      fun atom strm =
        case getc strm of
          NONE => raise Scan "Unexpected end of input"
        | SOME (#"(", strm) =>
          let val (exp, strm) = expr strm
          in
            case getc strm of
              SOME (#")", strm) => (exp, strm)
            | _ => raise Fail "Missing closing parenthesis"
          end
        | SOME (#"|", strm) => raise Scan "Unexpected character: |"
        | SOME (#"*", strm) => raise Scan "Unexpected character: *"
        | SOME (#")", strm) => raise Scan "Unexpected character: )"
        | SOME (#"\\", strm) =>
          (case escape getc strm of
            NONE => raise Scan "Invalid escape sequence"
          | SOME v => v)
        | SOME (c, strm) => (Re.Literal c, strm)

      and star strm =
        let val (atm, strm) = atom strm
        in
          case getc strm of
            SOME (#"*", strm) => (Re.Star atm, strm)
          | _ => (atm, strm)
        end

      and concat strm =
        let
          fun loop acc strm =
            case getc strm of
              NONE => (acc, strm)
            | SOME (#"|", _) => (acc, strm)
            | SOME (#")", _) => (acc, strm)
            | SOME _ =>
              let val (str, strm) = star strm
              in loop (str :: acc) strm
              end

          val (seq, strm) = loop [] strm
          val seq = (Re.Concat o Vector.fromList o rev) seq
        in
          (seq, strm)
        end

      and expr strm =
        let val (t, strm) = concat strm
        in
          case getc strm of
            SOME (#"|", strm) =>
              let val (t2, strm) = expr strm
              in (Re.Alt (t, t2), strm)
              end
          | _ => (t, strm)
        end
    in
      SOME (expr strm)
      handle Scan _ => NONE
    end
end
