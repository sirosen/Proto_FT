structure FlattenOps : sig

  val subToNFSub : Exp.sub -> Exp.nfsub

  val nfsubToFSub : Exp.nfsub -> Exp.fsub

  end = struct

    datatype sub = datatype Exp.sub
    datatype nfsub = datatype Exp.nfsub
    datatype fsub = datatype Exp.fsub

    fun subToNFSub (ARR_SUB(n)) = NF_ARR_SUB([n],[])
      | subToNFSub (TUP_SUB(n)) = NF_TUP_SUB(n)
      | subToNFSub (s as SUB_COMP(s1,s2)) = let
          fun opToList (SUB_COMP(s1,s2)) = (opToList s2) @ (opToList s1)
            | opToList s = [s]
          fun getArrSubs (ARR_SUB(n)::ss) = n::getArrSubs(ss)
            | getArrSubs (TUP_SUB(n)::ss) = getArrSubs(ss)
            | getArrSubs [] = []
            | getArrSubs _ = raise Fail "getArrSubs applied to invalid operand"
          fun getTupSubs (TUP_SUB(n)::ss) = n::getTupSubs(ss)
            | getTupSubs (ARR_SUB(n)::ss) = getTupSubs(ss)
            | getTupSubs [] = []
            | getTupSubs _ = raise Fail "getTupSubs applied to invalid operand"
          fun toNF [TUP_SUB(n)] = NF_TUP_SUB(n)
            | toNF (all as (TUP_SUB(n)::ss)) = NF_SUB_COMP(toNF ss,NF_TUP_SUB(n))
            | toNF (all as (ARR_SUB(n)::ss)) = NF_ARR_SUB(getArrSubs all,
                                                          getTupSubs all)
            | toNF _ = raise Fail "Empty operator or other failed match in Sub -> NFSub"
          val ss = opToList s
          in 
            toNF ss
          end

    fun nfsubToFSub (nfs as NF_TUP_SUB(n)) = F_TUP_SUB(n)
      | nfsubToFSub (nfs as NF_SUB_COMP(a,b)) =
          F_SUB_COMP(nfsubToFSub a, nfsubToFSub b)
      | nfsubToFSub (nfs as NF_ARR_SUB(ps,qs)) = let
          fun intermediateSub ([] : int list, [] : int list) : fsub list = []
            | intermediateSub ([],m::ms) =
                F_TUP_SUB(m)::intermediateSub([],ms)
            | intermediateSub (n::ns,[]) = [F_ARR_SUB(n::ns)]
               (* Tuples are on the outside, so they have
                * higher precedence than arrays *)
            | intermediateSub (n::ns,m::ms) = F_TUP_SUB(m)::intermediateSub(n::ns,ms)
          val subs = intermediateSub(ps,qs)
          in
            case subs
              of [] => raise Fail "Empty sub operator in NFSub -> FSub"
                (* Note the order of a and b! Mind bendy, but correct. *)
               | s::ss => foldl (fn (a,b) => F_SUB_COMP(b,a)) s ss
          end

  end
