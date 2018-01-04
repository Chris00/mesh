open Bigarray

let copy_vec v =
  let v' = Array1.create (Array1.kind v) (Array1.layout v) (Array1.dim v) in
  Array1.blit v v';
  v'

let copy_mat m =
  let m' = Array2.create (Array2.kind m) (Array2.layout m)
                         (Array2.dim1 m) (Array2.dim2 m) in
  Array2.blit m m';
  m'
