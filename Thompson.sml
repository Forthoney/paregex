structure Thompson =
struct
  type state_id = int

  datatype 'a transition =
    Epsilon of state_id
  | Literal of 'a * state_id
  | Split of state_id * state_id

  val fetchAndAdd = MLton.Parallel.fetchAndAdd

  structure Re = Regex

  fun numStates Re.Epsilon = 2
    | numStates (Re.Literal _) = 2
    | numStates (Re.Concat (l, r)) = numStates l + numStates r
    | numStates (Re.Alt (l, r)) = numStates l + numStates r + 2
    | numStates (Re.Star r) = numStates r + 2

  fun compile re =
    let
      val stateId = ref 0
      val bogus = Epsilon 0
      val trans = Array.array (numStates re, bogus) 

      fun fresh () =
        let val start = fetchAndAdd stateId 2
        in (start, start + 1)
        end
      
      fun loop (start, accept) re =
        case re of
          Re.Literal c =>
          let val (start, accept) = fresh ()
          in
            (Array.update (trans, start, Literal (c, accept)); (start, accept))
          end
        | Re.Epsilon =>
          let val (start, accept) = fresh ()
          in
            (Array.update (trans, start, Epsilon accept); (start, accept))
          end
        | Re.Alt (l, r) =>
          let
            val ((lStart, lAccept), (rStart, rAccept)) = ForkJoin.par (fn () => loop l, fn () => loop r)
            val (start, accept) = fresh ()
          in
            ( Array.update (trans, start, Split (lStart, rStart))
            ; Array.update (trans, lAccept, Epsilon accept)
            ; Array.update (trans, rAccept, Epsilon accept)
            ; (start, accept)
            )
          end
        | Re.Concat (l, r) =>
          let
            val ((lStart, lAccept), (rStart, rAccept)) = ForkJoin.par (fn () => loop l, fn () => loop r)
          in
            (Array.update (trans, lAccept, Epsilon rStart); (lStart, rAccept))
          end
        | Re.Star r =>
          let
            val (rStart, rAccept) = loop r
            val (start, accept) = fresh ()
          in
            ( Array.update (trans, rAccept, Split (rStart, accept))
            ; Array.update (trans, start, Split (rStart, accept))
            ; (start, accept)
            )
          end
    in
      loop re
    end
end

