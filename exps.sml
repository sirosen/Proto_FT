structure Exp = struct

    datatype ground_term = INT of int

    (* Shape tree. We can rethink this later. *)
    datatype shape = Node of shape list
                   | Lf of int * int

    and farray = FArray of ground_term list * shape
               | FArray_Tup of farray * farray

    (* nested flat array *)
    datatype nfarray = NFA_Tup of nfarray * nfarray
                     | NFA_Arr of nfarray list
                     | NFA_Lf of farray


    datatype sub = ARR_SUB of int
                 | TUP_SUB of int
                 | SUB_COMP of sub * sub
    and term = GROUND of ground_term
             | ARR of term list
             | TUP of term * term
             | APPLY_SUB of sub * term


    (* nested flat sub op *)
    datatype nfsub = NF_ARR_SUB of int list * int list
                   | NF_TUP_SUB of int
                   | NF_SUB_COMP of nfsub * nfsub
    (* nested flat term *)
    and nfterm = NF_GROUND of ground_term
               | NF_ARR of nfarray
               | NF_TUP of nfterm * nfterm
               | NF_APPLY_SUB of nfsub * nfterm

    (* flattened sub op *)
    datatype fsub = F_ARR_SUB of int list
                  | F_TUP_SUB of int
                  | F_SUB_COMP of fsub * fsub
    (* flattened term *)
    and fterm = F_GROUND of ground_term
              | F_ARR of farray
              | F_TUP of fterm * fterm
              | F_APPLY_SUB of fsub * fterm


  end
