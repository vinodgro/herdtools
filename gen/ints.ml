module O = struct
  type t = int
  let compare = Pervasives.compare
end
include MySet.Make(O)
