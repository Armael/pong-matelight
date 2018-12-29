val nb_columns : int
val nb_lines : int

type image

val superpose : image list -> image

val of_matrix : int array array -> image

type rectangle = {
  color : int;
  x : int; y : int;
  w : int; h : int;
}

val of_rectangles : rectangle list -> image

val rgb : r:int -> g:int -> b:int -> int
val get_rgb : int -> int * int * int

type conn

val connect : ?url:string -> ?port:int -> unit -> conn Lwt.t
val send_image : conn -> image -> unit Lwt.t
val disconnect : conn -> unit
