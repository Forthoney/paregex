structure Regex =
struct
  datatype 'a t =
    Literal of 'a
  | Concat of ('a t) vector
  | Alt of 'a t * 'a t
  | Star of 'a t

  fun scan getc strm =
    let
      exception ParseFail of string
      fun atom strm =
        case getc strm of
          NONE => raise ParseFail "Unexpected end of input"
        | SOME (#"(", strm) =>
          let val (exp, strm) = expr strm
          in
            case getc strm of
              SOME (#")", strm) => (exp, strm)
            | _ => raise Fail "Missing closing parenthesis"
          end
        | SOME (#"|", strm) => raise ParseFail "Unexpected character: |"
        | SOME (#"*", strm) => raise ParseFail "Unexpected character: *"
        | SOME (#")", strm) => raise ParseFail "Unexpected character: )"
        | SOME (c, strm) => (Literal c, strm)

      and star strm =
        let val (atm, strm) = atom strm
        in
          case getc strm of
            SOME (#"*", strm) => (Star atm, strm)
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
          val seq = (Concat o Vector.fromList o rev) seq
        in
          (seq, strm)
        end

      and expr strm =
        let val (t, strm) = concat strm
        in
          case getc strm of
            SOME (#"|", strm) =>
              let val (t2, strm) = expr strm
              in (Alt (t, t2), strm)
              end
          | _ => (t, strm)
        end
    in
      SOME (expr strm)
      handle ParseFail _ => NONE
    end

  fun prec (Literal _) = 4
    | prec (Star _) = 3
    | prec (Concat _) = 2
    | prec (Alt _) = 1

  fun toString (r : char t) : string =
    let
      fun help r parentPrec =
        let
          val currPrec = prec r
          val s =
            case r of
              Literal c => String.str c
            | Star inner => (help inner currPrec) ^ "*"
            | Concat rs =>
              Vector.foldl (fn (r, acc) => acc ^ help r currPrec) "" rs
            | Alt (r1, r2) =>
              (help r1 currPrec) ^ "|" ^ (help r2 currPrec)
        in
          if currPrec < parentPrec then "(" ^ s ^ ")" 
          else s
        end
    in
      help r 0
    end
end
