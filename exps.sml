structure Exp : sig

  type exp
  type const

  val bindToVar : Var.var * exp -> unit

  end = struct

    type var = Var.var

    datatype exp = E_Let of (var * exp * exp)
                 | E_Apply of (var * exp)
                 | E_Const of const

    and const = ARR of const array
              | FUN of var list -> exp
              | TUP of const * const 
              | INT of int
              | UNIT

    val varTbl = Var.IDTbl.mkTable (1024, Fail "Unbound variable!") : exp Var.IDTbl.hash_table

    fun bindToVar (v,e) = Var.IDTbl.insert varTbl (v,e)

  end
