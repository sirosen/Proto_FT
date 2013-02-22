structure Flatten : sig

  val mkFlatExp : Exp.exp -> Exp.exp
  val constToNFA : Exp.const * Ty.ty -> Exp.NFArray

  end = struct

    datatype ty = datatype Ty.ty
    datatype const = datatype Exp.const
    datatype exp = datatype Exp.exp

    datatype NFArray = datatype Exp.NFArray
    datatype Shape = datatype Exp.Shape
    datatype FArray = datatype Exp.FArray

    val unzip = Utils.unzip
    val zip = Utils.zip

    (* constToNFA must have the typing information for the NFA
     * in order to be able to make empty arrays with correct
     * tuple arity. *)
    fun constToNFA (e,t) = let
        fun unzipConstList (cs : const list) = let
            fun breakTup c =
              case c
                of TUP(a,b) => (a,b)
                 | _ => raise Fail "Expected tuple! Type mismatch!"
          in
            unzip (map breakTup cs)
          end
        fun arrToNFA (cs : const list, t : ty) =
             NFA_Arr (map constToNFA (map (fn x => (x,t)) cs))
      in
        case (e,t)
          of (ARR(cs),ARR_TY(t')) =>
               (case t'
                  of ARR_TY(_) => arrToNFA(cs,t')
                   | TUP_TY(tp,tq) =>
                     let
                       val (ps,qs) = unzipConstList cs
                     in
                       NFA_Tup (constToNFA (ARR(ps),ARR_TY(tp)),
                                constToNFA (ARR(qs),ARR_TY(tq)))
                     end
                   | _ => NFA_Lf(FArray(cs,Lf(0,length cs)))
               (*end case*))
           | _ => raise Fail "Trying to make a non-array expression into an NFA or type mismatch."
      end

    fun mkFlatExp e =
      case e
        of E_Let(v,e1,e2) => raise Fail "todo"
         | E_Apply(v,e1) => raise Fail "todo"
         | E_Const(c) => raise Fail "todo"
         (*| _ => raise Fail "Invalid expression in Flatten.mkFlatExp"*)

  end
