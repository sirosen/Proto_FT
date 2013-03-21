structure Flatten : sig

  val termToNFTerm : Exp.term * Ty.ty -> Exp.nfterm

  val nftermToFTerm : Exp.nfterm -> Exp.fterm

  end = struct

    structure FOps = FlattenOps

    datatype ty = datatype Ty.ty
    datatype ground_ty = datatype Ty.ground_ty

    datatype term = datatype Exp.term
    datatype nfterm = datatype Exp.nfterm
    datatype fterm = datatype Exp.fterm
    datatype gterm = datatype Exp.ground_term

    datatype shape = datatype Exp.shape
    datatype farray = datatype Exp.farray
    datatype nfarray = datatype Exp.nfarray


    fun termToNFTerm (t,ty) =
      case (t,ty)
        of (GROUND(g),_) => NF_GROUND(g)
         | (ARR(ts),ARR_TY(ty')) => raise Fail "todo"
         | (TUP(t1,t2),TUP_TY(ty1,ty2)) =>
             NF_TUP(termToNFTerm (t1,ty1), termToNFTerm (t2,ty2))
         | _ => raise Fail "Type mismatch in SourceTerm->NFTerm"

    fun nftermToFTerm t =
      let
        fun flattenNFA (NFA_Tup(ns,ms)) = FArray_Tup(flattenNFA ns,
                                                   flattenNFA ms)
          | flattenNFA (NFA_Arr(nss)) = raise Fail
              "Insufficiently flattened array in NFA->FA"
          | flattenNFA (NFA_Lf(fa)) = fa
      in
        case t
          of NF_GROUND(g) => F_GROUND(g)
           | NF_ARR(ns) => F_ARR(flattenNFA ns)
           | NF_TUP(t1,t2) => F_TUP(nftermToFTerm t1,
                                    nftermToFTerm t2)
      end


  end
