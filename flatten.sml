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

    fun termToNFTerm t = raise Fail "todo"
    fun nftermToFTerm t = raise Fail "todo"

    fun subToNFSub s = let
      fun getNFArrSub (nfs as NF_ARR_SUB(ns,ms)) = ns
      fun getNFTupSub (nfs as NF_ARR_SUB(ns,ms)) = ms
      (* It is important that we concat with nfs2 first,
       * and nfs1 second, so that we apply operations by
       * walking down the lists *)
      fun nfSubMerge (nfs1,nfs2) =
        NF_ARR_SUB(
            (getNFArrSub nfs2) @ (getNFArrSub nfs1),
            (getNFTupSub nfs2) @ (getNFTupSub nfs1)
            )
      in
        case s
          of ARR_SUB(n) => NF_ARR_SUB([n],[])
           | TUP_SUB(n) => NF_ARR_SUB([],[n])
           | OP_COMP(s1,s2) => nfSubMerge (subToNFSub s1,
                                           subToNFSub s2)
      end

    fun nfsubToFSub nfs = raise Fail "todo"

  end
