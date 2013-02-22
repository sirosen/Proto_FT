structure Utils : sig
  val zip : 'a list * 'b list -> ('a * 'b) list
  val unzip : ('a * 'b) list -> 'a list * 'b list
  end = struct

    fun unzip (xs : ('a * 'b) list) = (map #1 xs, map #2 xs)

    fun zip (x::xs,y::ys) = (x,y)::(zip(xs,ys))
    fun zip (xs,ys) = []

  end
