structure Ty =
  struct

    datatype ty = ARR_TY of ty
                | FUN_TY of ty * ty
                | TUP_TY of ty * ty
                | INT_TY
                | UNIT_TY

  end
