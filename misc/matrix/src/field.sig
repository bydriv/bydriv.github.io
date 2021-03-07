signature FIELD = sig
  include RING
  val inverse : t -> t option
end
