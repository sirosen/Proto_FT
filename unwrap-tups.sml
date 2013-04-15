structure UnwrapTups : sig

  val unwrap : Exp.nfterm * Ty.ty -> Exp.nfterm * Ty.ty

  end =
  struct

    type indexer = int list list

    fun unwrapTy ty =
      let
        fun addArr ty =
          case ty
            of TUP_TY(a,b) => TUP_TY(addArr a, addArr b)
             | _ => ARR_TY(ty)
      in
        case ty
          of GROUND_TY(_) => ty
           | TUP_TY(a,b) => TUP_TY(unwrapTy a, unwrapTy b)
           | ARR_TY(innerTy) => addArr (unwrapTy innerTy)
      end


    (* Assumes that the type has been unwrapped *)
    fun getIndexer ty = raise Fail "todo"

    fun applyIndexer ind target = raise Fail "todo"

    fun unwrap ((ns,ty) as (NFA_Lf(_),ARR_TY(_))) = (ns,ty)
      | unwrap (NFA_TUP(ps,qs),TUP_TY(tp,tq)) =
          let
            val (ps',tp') = unwrap (ps,tp)
            val (qs',tq') = unwrap (qs,tq)
          in
            (NFA_TUP(ps',qs'),TUP_TY(tp',tq'))
          end
      | unwrap (NFA_Arr(ns),ARR_TY(ty)) =
          let
            val inner = map (fn n => unwrap (n,ty)) ns
            val ty' = unwrapTy ty
            val ind = getIndexer ty'
          in
            (applyIndexer ind (map fst inner), ty')
          end

  end
