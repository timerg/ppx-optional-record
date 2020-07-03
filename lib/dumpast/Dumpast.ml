type a = { a1: string; a2: int;}
type a_op = { a1: string option; a2: int option;}

(* let f = (a: a): a_op => {
  {

  }
} *)

let a: a = {
  a1 = "123" ;
  a2 = 223 ;
}

let toOp (ag: a): a_op =
  {
    a1 = Some(ag.a1);
    a2 = Some(ag.a2);
  }

exception FieldLost of string

let fromOp (aop: a_op): a =
  let f v =  match v with | None -> raise(FieldLost("123")) | Some(v) -> v in
  {
    a1 = f(aop.a1);
    a2 = match aop.a2 with | None -> raise(FieldLost("123")) | Some(v)->v;
  }