structure Ty =
  struct

    datatype ground_ty = INT_TY

    and ty = ARR_TY of ty
           | TUP_TY of ty * ty
           | GROUND_TY of ground_ty

  end
