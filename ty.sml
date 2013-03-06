structure Ty =
  struct

    datatype ty = ARR_TY of ty
                | TUP_TY of ty * ty
                | INT_TY

  end
