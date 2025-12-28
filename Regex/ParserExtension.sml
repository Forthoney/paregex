structure ParserExtension :
sig
  structure Barebones : REGEX_PARSER_EXTENSION
  structure Simple : REGEX_PARSER_EXTENSION
  structure Standard : REGEX_PARSER_EXTENSION
end =
struct
  structure Re = Basis

  val isMeta = Char.contains "()|*"

  structure Barebones : REGEX_PARSER_EXTENSION =
  struct
    fun escape getc strm =
      case getc strm of
        NONE => NONE
      | SOME (c, strm) =>
        if isMeta c then SOME (Re.Literal c, strm)
        else
          case c of
            #"a" => SOME (Re.Literal #"\a", strm)
          | #"f" => SOME (Re.Literal #"\f", strm)
          | #"t" => SOME (Re.Literal #"\t", strm)
          | #"n" => SOME (Re.Literal #"\n", strm)
          | #"r" => SOME (Re.Literal #"\r", strm)
          | #"v" => SOME (Re.Literal #"\v", strm)
          | _ => NONE

    fun postfix prev getc strm =
      case getc strm of
        SOME (#"*", strm) => SOME (Re.Star prev, strm)
      | _ => NONE
  end

  structure Simple : REGEX_PARSER_EXTENSION =
  struct
    val escape = Barebones.escape    

    fun postfix prev getc strm =
      case getc strm of
        SOME (#"*", strm) => SOME (Re.Star prev, strm)
      | SOME (#"?", strm) => SOME (Re.Alt (prev, Re.Concat (Vector.fromList [])), strm)
      | SOME (#"+", strm) => SOME (Re.Concat (Vector.fromList [prev, Re.Star prev]), strm)
      | _ => NONE
  end

  structure Standard : REGEX_PARSER_EXTENSION =
  struct
    val escape = Barebones.escape

    fun postfix prev getc strm =
      case Simple.postfix prev getc strm of
        SOME v => SOME v
      | NONE =>
        case getc strm of
          SOME (#"{", strm) =>
          (case Int.scan StringCvt.DEC getc strm of
            SOME (i, strm) =>
            if i >= 0 then
              case getc strm of
                SOME (#"}", strm) => SOME (Re.Concat (Array.vector (Array.array (i, prev))), strm)
              | _ => NONE
            else NONE
          | _ => NONE)
        | _ => NONE
  end
end
