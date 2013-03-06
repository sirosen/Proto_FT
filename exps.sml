structure Exp = struct

    datatype ground_term = INT of int

    (* Shape tree. We can rethink this later. *)
    datatype shape = Node of shape list
                   | Lf of int * int

    and farray = FArray of int list * shape

    (* nested flat array *)
    datatype nfarray = NFA_Tup of nfarray * nfarray
                     | NFA_Arr of nfarray list
                     | NFA_Lf of farray


    datatype basic_op = ARR_SUB of int
                | TUP_SUB of int
                | OP_COMP of basic_op * basic_op
    and term = GROUND of ground_term
             | ARR of term list
             | TUP of term * term
             | APPLY_SUB of basic_op * term


    (* nested flat op *)
    datatype nfop = NF_ARR_SUB of int list * int list
                  | NF_TUP_SUB of int
    (* nested flat term *)
    and nfterm = NF_GROUND of ground_term
               | NF_ARR of nfarray
               | NF_TUP of nfterm * nfterm
               | NF_APPLY_SUB of nfop * nfterm

    (* flattened op *)
    datatype fop = F_ARR_SUB of int list
                 | F_TUP_SUB of int
                 | F_OP_COMP of fop * fop
    (* flattened term *)
    and fterm = F_GROUND of ground_term
              | F_ARR of farray
              | F_TUP of fterm * fterm
              | F_APPLY_SUB of fop * fterm


(*
    val varTbl = Var.IDTbl.mkTable (1024, Fail "Unbound variable!") : exp Var.IDTbl.hash_table

    fun bindToVar (v,e) = Var.IDTbl.insert varTbl (v,e)
*)

  end
