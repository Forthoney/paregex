signature THOMPSON_NFA =
sig
  eqtype state_id

  datatype 'a transition =
    Epsilon of state_id
  | Literal of 'a * state_id
  | Split of state_id * state_id

  type 'a transition_table 

  val compile : 'a Regex.t -> 'a transition_table
end
