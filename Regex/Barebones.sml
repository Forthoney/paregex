structure Barebones : REGEX_PARSER_EXTENSION =
struct
  structure Re = Regex

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

  fun postfix prev getc strm =
    case getc strm of
      SOME (#"*", strm) => SOME (Re.Star prev, strm)
    | _ => NONE
end
