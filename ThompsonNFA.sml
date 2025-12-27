structure ThompsonNFA : THOMPSON_NFA =
struct
  type state_id = int

  datatype 'a transition =
    Epsilon of state_id
  | Literal of 'a * state_id
  | Split of state_id * state_id

  type 'a transition_table = ('a transition) array

  val fetchAndAdd = MLton.Parallel.fetchAndAdd

  structure Re = Regex

  fun numStates re =
    let
      fun loop (re, acc) =
        case re of
          Re.Literal _ => acc
        | Re.Star r => loop (r, acc + 2)
        | Re.Alt (l, r) => loop (r, loop (l, acc + 4))
        | Re.Concat rs => 
          case Vector.length rs of
            0 => acc
          | 1 => loop (Vector.sub (rs, 0), acc)
          | n => Vector.foldl loop (acc + n + 1) rs
    in
      loop (re, 2)
    end

  fun compile re =
    let
      val stateId = ref 2
      val bogus = Epsilon 0
      val n = numStates re
      val _ = print (Int.toString n ^ " states\n")
      val trans = Array.array (n, bogus)

      fun loop (start, accept) re =
        case re of
          Re.Literal c => Array.update (trans, start, Literal (c, accept))
        | Re.Alt (l, r) =>
          let
            val lStart = fetchAndAdd stateId 4
            val (lAccept, rStart, rAccept) = (lStart + 1, lStart + 2, lStart + 3)
          in
            ( ForkJoin.par (fn () => loop (lStart, lAccept) l, fn () => loop (rStart, rAccept) r)
            ; Array.update (trans, start, Split (lStart, rStart))
            ; Array.update (trans, lAccept, Epsilon accept)
            ; Array.update (trans, rAccept, Epsilon accept)
            )
          end
        | Re.Star r =>
          let
            val rStart = fetchAndAdd stateId 2
            val rAccept = rStart + 1
          in
            ( Array.update (trans, rAccept, Split (rStart, accept))
            ; Array.update (trans, start, Split (rStart, accept))
            )
          end
        | Re.Concat rs =>
          case Vector.length rs of
            0 => Array.update (trans, start, Epsilon accept)
          | 1 => loop (start, accept) (Vector.sub (rs, 0))
          | n =>
            let
              val innerStart = fetchAndAdd stateId (n + 1)
              val innerAccept = innerStart + n
            in
              ( ForkJoin.parform (0, n) (fn i => loop (i + innerStart, i + innerStart + 1) (Vector.sub (rs, i)))
              ; Array.update (trans, start, Epsilon innerStart)
              ; Array.update (trans, innerAccept, Epsilon accept)
              )
            end
    in
      (loop (0, 1) re; trans)
    end
end

