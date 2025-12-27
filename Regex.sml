structure Regex =
struct
  datatype 'a t =
    Epsilon
  | Literal of 'a
  | Concat of 'a t * 'a t
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
              in loop (Concat (acc, str)) strm
              end

          val (seq, strm) = loop Epsilon strm
          val seq = 
            case seq of
              Concat (regex, Epsilon) => regex
            | _ => seq
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

  fun prec Epsilon = 4
    | prec (Literal _) = 4
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
              Epsilon => "" 
            | Literal c => String.str c
            | Star inner => 
                (help inner currPrec) ^ "*"
            | Concat (r1, r2) => 
                (help r1 currPrec) ^ (help r2 currPrec)
            | Alt (r1, r2) => 
                (help r1 currPrec) ^ "|" ^ (help r2 currPrec)
        in
          if currPrec < parentPrec then  "(" ^ s ^ ")" 
          else s
        end
    in
      help r 0
    end
end
