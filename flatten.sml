structure Flatten : sig

  type FArray
  type NFArray

  val mkFlatExp : Exp.exp -> Exp.exp

  end = struct

    type ty = Ty.ty
    type var = Var.var

    datatype exp = datatype Exp.exp
    datatype const = datatype Exp.const

    (* Shape tree. We can rethink this later. *)
    datatype Shape = Node of Shape list
                   | Lf of int * int

    datatype FArray = FArray of const array * Shape

    datatype NFArray = NFA_Tup of NFArray * NFArray
                     | NFA_Arr of NFArray array
                     | NFA_Lf of FArray

    fun mkFlatExp e =
      case e
        of E_Let(v,e1,e2) => raise Fail "todo"
         | E_Apply(v,e1) => raise Fail "todo"
         | E_Const(c) => raise Fail "todo"
         (*| _ => raise Fail "Invalid expression in Flatten.mkFlatExp"*)

  end
