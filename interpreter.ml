type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp
;;

exception Eval_error;;

(* Values: True, False, Num(n), Pair(v1,v2) where v1 and v2 are values *)
let rec is_value (e : exp) : bool =
  match e with
  | True -> true
  | False -> true
  | Num _ -> true
  | Pair (e1, e2) -> is_value e1 && is_value e2
  | _ -> false
;;

let rec step (e : exp) : exp =
  match e with
  (* If *)
  | If (c, texp, fexp) ->
      if not (is_value c) then
        If (step c, texp, fexp)
      else
        (match c with
         | True -> texp
         | False -> fexp
         | _ -> raise Eval_error)  (* condition is a value but not boolean *)

  (* IsZero *)
  | IsZero e1 ->
      if not (is_value e1) then
        IsZero (step e1)
      else
        (match e1 with
         | Num n -> if n = 0 then True else False
         | _ -> raise Eval_error)  (* value but not an integer *)

  (* Plus *)
  | Plus (e1, e2) ->
      if not (is_value e1) then
        Plus (step e1, e2)
      else
        (match e1 with
         | Num n1 ->
             if not (is_value e2) then
               Plus (e1, step e2)
             else
               (match e2 with
                | Num n2 -> Num (n1 + n2)
                | _ -> raise Eval_error) (* second is value but not integer *)
         | _ -> raise Eval_error)       (* first is value but not integer *)

  (* Pair *)
  | Pair (e1, e2) ->
      if not (is_value e1) then
        Pair (step e1, e2)
      else if not (is_value e2) then
        Pair (e1, step e2)
      else
        raise Eval_error  (* step on a value (pair of values) *)

  (* Fst *)
  | Fst e1 ->
      if not (is_value e1) then
        Fst (step e1)
      else
        (match e1 with
         | Pair (v1, v2) when is_value v1 && is_value v2 -> v1
         | _ -> raise Eval_error)  (* value but not a pair *)

  (* Snd *)
  | Snd e1 ->
      if not (is_value e1) then
        Snd (step e1)
      else
        (match e1 with
         | Pair (v1, v2) when is_value v1 && is_value v2 -> v2
         | _ -> raise Eval_error)  (* value but not a pair *)

  (* Any value: True/False/Num/Pair(v,v) *)
  | _ ->
      if is_value e then raise Eval_error
      else raise Eval_error
;;

let rec multi_step (e : exp) : exp =
  if is_value e then e
  else multi_step (step e)
;;