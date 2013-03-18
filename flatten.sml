structure Flatten : sig

  val termToNFTerm : Exp.term -> Exp.nfterm
  val subToNFSub : Exp.sub -> Exp.nfsub

  val nftermToFTerm : Exp.nfterm -> Exp.fterm
  val nfsubToFSub : Exp.nfsub -> Exp.fsub

  end = struct


    datatype sub = datatype Exp.sub
    datatype nfsub = datatype Exp.nfsub
    datatype fsub = datatype Exp.fsub

    datatype term = datatype Exp.term
    datatype nfterm = datatype Exp.nfterm
    datatype fterm = datatype Exp.fterm
    datatype gterm = datatype Exp.ground_term

    datatype shape = datatype Exp.shape
    datatype farray = datatype Exp.farray
    datatype nfarray = datatype Exp.nfarray

    fun subToNFSub s = let
      fun getNFArrSub (nfs as NF_ARR_SUB(ns,ms)) = ns
        | getNFArrSub (nfs as NF_TUP_SUB(_)) = []
        | getNFArrSub (nfs as NF_SUB_COMP(a,b)) =
            getNFArrSub(b) @ getNFArrSub(a)
      fun getNFTupSub (nfs as NF_ARR_SUB(ns,ms)) = ms
        | getNFTupSub (nfs as NF_TUP_SUB(n)) = [n]
        | getNFTupSub (nfs as NF_SUB_COMP(a,b)) =
            getNFTupSub(b) @ getNFTupSub(a)
      (* It is important that we concat with nfs2 first,
       * and nfs1 second, so that we apply operations by
       * walking down the lists.
       * Remember that if we write h = (f o g) then h applies
       * g first, then f; i.e. read R to L. *)
      fun nfsMerge (nfs1,nfs2) =
        NF_ARR_SUB(
            (getNFArrSub nfs2) @ (getNFArrSub nfs1),
            (getNFTupSub nfs2) @ (getNFTupSub nfs1)
            )
      (* Correctness here becomes non-obvious, so a short explanation:
       * Any operator that looks like TUP o ARR is actually an
       * array subscript, because we index into an array before indexing
       * into tuples.
       * What about TUP o ARR o TUP? We want to catch these cases, which
       * SUB_COMP nodes might not handle gracefully. *)
      fun hasArrSub (s as ARR_SUB(_)) = true
        | hasArrSub (s as TUP_SUB(_)) = false
        | hasArrSub (s as SUB_COMP(a,b)) = hasArrSub a orelse
                                           hasArrSub b
      fun hasNFArrSub (s as NF_ARR_SUB(_)) = true
        | hasNFArrSub (s as NF_TUP_SUB(_)) = false
        | hasNFArrSub (s as NF_SUB_COMP(a,b)) = hasNFArrSub a orelse
                                                hasNFArrSub b
      in
        case s
          of ARR_SUB(n) => NF_ARR_SUB([n],[])
           | TUP_SUB(n) => NF_TUP_SUB(n)
           | SUB_COMP(s1,s2) =>
               let
                 val nfs1 = subToNFSub s1
                 val nfs2 = subToNFSub s2
               in
                 case nfs2
                   of NF_ARR_SUB(ns,ms) =>
                        nfsMerge (nfs1, NF_ARR_SUB(ns,ms))
                    | NF_TUP_SUB(n) =>
                        NF_SUB_COMP(nfs1, NF_TUP_SUB(n))
                    | NF_SUB_COMP(a,b) =>
                        if hasNFArrSub b then
                          nfsMerge (nfs1, nfs2)
                        else if hasNFArrSub a then
                          NF_SUB_COMP(nfsMerge (nfs1,a), b)
                        else
                          NF_SUB_COMP(nfs1,nfs2)
               end
      end

    fun nfsubToFSub (nfs as NF_TUP_SUB(n)) = F_TUP_SUB(n)
      | nfsubToFSub (nfs as NF_SUB_COMP(a,b)) =
          F_SUB_COMP(nfsubToFSub a, nfsubToFSub b)
      | nfsubToFSub (nfs as NF_ARR_SUB(ps,qs)) = let
          fun interSub ([] : int list, [] : int list) : fsub list = []
            | interSub ([],m::ms) = F_TUP_SUB(m)::interSub([],ms)
            | interSub (n::ns,[]) = [F_ARR_SUB(n::ns)]
               (* Tuples are on the outside, so they have
                * higher precedence than arrays *)
            | interSub (n::ns,m::ms) = F_TUP_SUB(m)::interSub(n::ns,ms)
          val subs = interSub(ps,qs)
          in
            case subs
              of [] => raise Fail "Empty sub operator in NFSub -> FSub"
               | s::ss => foldl (fn (a,b) => F_SUB_COMP(a,b)) s ss
          end

    fun termToNFTerm t =
      case t
        of GROUND(g) => NF_GROUND(g)
         | ARR(ts) => raise Fail "todo"
         | TUP(t1,t2) => NF_TUP(termToNFTerm t1, termToNFTerm t2)
         | APPLY_SUB(s,t') => NF_APPLY_SUB(subToNFSub s,
                                           termToNFTerm t')

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
           | NF_APPLY_SUB(s,t') => F_APPLY_SUB(nfsubToFSub s,
                                               nftermToFTerm t')
      end


  end
