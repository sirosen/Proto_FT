structure Exp =
  struct

    type var = Var.var

    datatype exp = E_Let of (var * exp * exp)
                 | E_Apply of (var * exp)
                 | E_Const of const

    and const = ARR of const array
              | FUN of var list -> exp
              | TUP of const * const 
              | INT of int
              | UNIT

  end
