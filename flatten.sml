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


    (* Takes a list of terms and the type of those terms
     * The terms represent an array, and the type is the
     * type over which the array datatype is parametrized. *)
    fun arrToNFArr ty ts =
      let
        fun unwrapGround (GROUND(gt)) = gt
          | unwrapGround _ =
              raise Fail "unwrapGround applied to non-ground term"
        fun unwrapTup (TUP(t1,t2)) = (t1,t2)
          | unwrapTup _ =
              raise Fail "unwrapTup applied to non-tuple term"
        fun unwrapArr (ARR(ts)) = ts
          | unwrapArr _ =
              raise Fail "unwrapArr applied to non-array term"
      in
        case ty
          of GROUND_TY(_) =>
               NFA_Lf(FArray(map unwrapGround ts,
                             Lf(0,length ts)))
           | TUP_TY(ty1,ty2) =>
               let
                 val (t1s,t2s) = Utils.unzip (map unwrapTup ts)
               in
                 NFA_Tup(arrToNFArr ty1 t1s,
                         arrToNFArr ty2 t2s)
               end
           | ARR_TY(ty'') =>
               NFA_Arr(map ((arrToNFArr ty'') o unwrapArr) ts)
      end

    fun termToNFTerm (t,ty) =
      case (t,ty)
        of (GROUND(g),GROUND_TY(_)) => NF_GROUND(g)
         | (ARR(ts),ARR_TY(ty')) => NF_ARR(arrToNFArr ty' ts)
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
