structure Flatten : sig

  val termToNFTerm : Exp.term -> Exp.nfterm
  val opToNFOp : Exp.basic_op -> Exp.nfop

  val nftermToFTerm : Exp.nfterm -> Exp.fterm
  val nfopToFOp : Exp.nfop -> Exp.fop

  end = struct


    datatype basic_op = datatype Exp.basic_op
    datatype nfop = datatype Exp.nfop
    datatype fop = datatype Exp.fop

    datatype term = datatype Exp.term
    datatype nfterm = datatype Exp.nfterm
    datatype fterm = datatype Exp.fterm
    datatype gterm = datatype Exp.ground_term

    datatype shape = datatype Exp.shape
    datatype farray = datatype Exp.farray
    datatype nfarray = datatype Exp.nfarray

    fun termToNFTerm t = raise Fail "todo"
    fun nftermToFTerm t = raise Fail "todo"
    fun opToNFOp b_op = raise Fail "todo"
    fun nfopToFOp nf_op = raise Fail "todo"

  end
