structure FlattenOps : sig

  val subToNFSub : Exp.sub -> Exp.nfsub

  val nfsubToFSub : Exp.nfsub -> Exp.fsub

  end = struct

    datatype sub = datatype Exp.sub
    datatype nfsub = datatype Exp.nfsub
    datatype fsub = datatype Exp.fsub

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

    fun hasArrSub (s as ARR_SUB(_)) = true
      | hasArrSub (s as TUP_SUB(_)) = false
      | hasArrSub (s as SUB_COMP(a,b)) = hasArrSub a orelse
                                         hasArrSub b
    fun hasNFArrSub (s as NF_ARR_SUB(_)) = true
      | hasNFArrSub (s as NF_TUP_SUB(_)) = false
      | hasNFArrSub (s as NF_SUB_COMP(a,b)) = hasNFArrSub a orelse
                                              hasNFArrSub b

    (* Correctness here becomes non-obvious, so a short explanation:
     * Any operator that looks like TUP o ARR is actually an
     * array subscript, because we index into an array before indexing
     * into tuples.
     * What about TUP o ARR o TUP? We want to catch these cases, which
     * SUB_COMP nodes might not handle gracefully. *)
    fun subToNFSub (ARR_SUB(n)) = NF_ARR_SUB([n],[])
      | subToNFSub (TUP_SUB(n)) = NF_TUP_SUB(n)
      | subToNFSub (SUB_COMP(s1,s2)) = let
          val nfs1 = subToNFSub s1
          val nfs2 = subToNFSub s2
          in
            case nfs2
              of NF_ARR_SUB(ns,ms) => nfsMerge (nfs1, nfs2)
               | NF_TUP_SUB(n) => NF_SUB_COMP(nfs1, nfs2)
               | NF_SUB_COMP(a,b) =>
                   if hasNFArrSub b then
                     nfsMerge (nfs1, nfs2)
                   else if hasNFArrSub a then
                     NF_SUB_COMP(nfsMerge (nfs1,a), b)
                   else
                     NF_SUB_COMP(nfs1,nfs2)
          end


    fun intermediateSub ([] : int list, [] : int list) : fsub list = []
      | intermediateSub ([],m::ms) =
          F_TUP_SUB(m)::intermediateSub([],ms)
      | intermediateSub (n::ns,[]) = [F_ARR_SUB(n::ns)]
         (* Tuples are on the outside, so they have
          * higher precedence than arrays *)
      | intermediateSub (n::ns,m::ms) = F_TUP_SUB(m)::intermediateSub(n::ns,ms)

    fun nfsubToFSub (nfs as NF_TUP_SUB(n)) = F_TUP_SUB(n)
      | nfsubToFSub (nfs as NF_SUB_COMP(a,b)) =
          F_SUB_COMP(nfsubToFSub a, nfsubToFSub b)
      | nfsubToFSub (nfs as NF_ARR_SUB(ps,qs)) = let
          val subs = intermediateSub(ps,qs)
          in
            case subs
              of [] => raise Fail "Empty sub operator in NFSub -> FSub"
                (* Note the order of a and b! Mind bendy, but correct. *)
               | s::ss => foldl (fn (a,b) => F_SUB_COMP(b,a)) s ss
          end

  end
