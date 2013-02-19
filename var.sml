structure Var :> sig

  type var

  structure IDTbl : MONO_HASH_TABLE where type Key.hash_key = var

  val newVar : Ty.ty * string -> var
  val typeOf : var -> Ty.ty
  val nameOf : var -> Atom.atom

  end = struct

    type ty = Ty.ty

    datatype var = V of {
      id : word,
      ty : ty,
      name : Atom.atom
      }


    structure IDTbl = HashTableFn(
      struct
          type hash_key = var
          val hashVal = fn (V{id, ...}) => id
          fun sameKey (V{id,...},V{id=id',...}) = (id = id')
      end
    )

    val cnt = ref 0w0
    fun nextID () = (cnt:=(!cnt)+0w1; !cnt)



    fun newVar (ty,name) = V{
      id = nextID(),
      ty = ty,
      name = Atom.atom name
      }

    fun typeOf (V{ty, ...}) = ty
    fun nameOf (V{name, ...}) = name

  end
