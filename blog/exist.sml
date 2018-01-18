signature MONOID = sig
  type m
  val empty : m
  val append : m * m -> m
end

structure AddMonoid :> MONOID = struct
  type m = int
  val empty = 0
  val append = op +
end

structure MulMonoid :> MONOID = struct
  type m = int
  val empty = 1
  val append = op *
end

structure StrMonoid :> MONOID = struct
  type m = string
  val empty = ""
  val append = op ^
end
