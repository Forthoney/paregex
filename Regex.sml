structure Regex =
struct
  datatype 'a t =
    Literal of 'a
  | Concat of ('a t) vector
  | Alt of 'a t * 'a t
  | Star of 'a t

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
              Literal c => Char.toCString c
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
