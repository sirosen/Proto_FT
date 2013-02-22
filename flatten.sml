structure Flatten : sig

  val flattenConst : Exp.const * Ty.ty -> Exp.const

  end = struct

    datatype ty = datatype Ty.ty
    datatype const = datatype Exp.const

    datatype NFArray = datatype Exp.NFArray
    datatype Shape = datatype Exp.Shape
    datatype FArray = datatype Exp.FArray

    (* constToNFA must have the typing information for the NFA
     * in order to be able to make empty arrays with correct
     * tuple arity. *)
    fun constToNFA (c,t) = let
        fun unzipConstList (cs : const list) = let
            fun breakTup c =
              case c
                of TUP(a,b) => (a,b)
                 | _ => raise Fail "Expected tuple! Type mismatch!"
          in
            Utils.unzip (map breakTup cs)
          end
        fun arrToNFA (cs : const list, t : ty) =
             NFA_Arr (map constToNFA (map (fn x => (x,t)) cs))
      in
        case c
          of ARR(cs) =>
               (case t
                  of ARR_TY(_) => arrToNFA(cs,t)
                   | TUP_TY(tp,tq) =>
                     let
                       val (ps,qs) = unzipConstList cs
                     in
                       NFA_Tup (constToNFA (ARR(ps),ARR_TY(tp)),
                                constToNFA (ARR(qs),ARR_TY(tq)))
                     end
                   | _ => NFA_Lf(FArray(cs,Lf(0,length cs)))
               (*end case*))
           | _ => raise Fail "Trying to make a non-array constant into an NFA."
      end

    fun flattenConst (c,t) =
      case t
        of ARR_TY(t') => NFArrayConst(constToNFA (c,t'))
         | _ => c

  end
