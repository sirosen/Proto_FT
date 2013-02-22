structure Exp : sig

  type exp
  type const
  type Shape
  type FArray
  type NFArray

  val bindToVar : Var.var * exp -> unit

  end = struct

    type var = Var.var

    datatype exp = E_Let of (var * exp * exp)
                 | E_Apply of (var * exp)
                 | E_Const of const

    and const = ARR of const list
              | FUN of var list -> exp
              | TUP of const * const 
              | INT of int
              | UNIT

    (* Shape tree. We can rethink this later. *)
    datatype Shape = Node of Shape list
                   | Lf of int * int

    datatype FArray = FArray of const list * Shape

    datatype NFArray = NFA_Tup of NFArray * NFArray
                     | NFA_Arr of NFArray list
                     | NFA_Lf of FArray



    val varTbl = Var.IDTbl.mkTable (1024, Fail "Unbound variable!") : exp Var.IDTbl.hash_table

    fun bindToVar (v,e) = Var.IDTbl.insert varTbl (v,e)

  end
