let rgb ~r ~g ~b =
  let r = r land 0xFF in
  let g = g land 0xFF in
  let b = b land 0xFF in
  r lor (g lsl 8) lor (b lsl 16)

let get_rgb n =
  (n land 0xFF, (n lsr 8) land 0xFF, (n lsr 16) land 0xFF)

(* screen dimensions: 40 x 16 *)

let nb_columns = 40
let nb_lines = 16

type image = Bytes.t -> unit

let image_buffer_size = 1924

let superpose l = fun b ->
  List.iter (fun img -> img b) l

let mk_buffer () =
  Bytes.make image_buffer_size (Char.chr 0)

(* Addressing:
0  1  2  3 ... 39
40 41 42 43 ...
...
*)

let write_pixel buf x y p =
  (* 3 bytes per pixel *)
  let offset = 3 * (40 * y + x) in
  if 0 <= x && x < nb_columns && 0 <= y && y < nb_lines then begin
    let (r, g, b) = get_rgb p in
    Bytes.set buf offset (Char.chr r);
    Bytes.set buf (offset + 1) (Char.chr g);
    Bytes.set buf (offset + 2) (Char.chr b)
  end else
    Printf.eprintf "WARN: Writing outside of the screen: %d, %d\n" x y

let of_matrix (m: int array array): image = fun b ->
  for x = 0 to Array.length m - 1 do
    for y = 0 to Array.length m.(0) - 1 do
      write_pixel b x y m.(x).(y)
    done
  done

type rectangle = {
  color : int;
  x : int; y : int;
  w : int; h : int;
}

let of_rectangles (l: rectangle list): image = fun b ->
  let write_rectangle { color; x; y; w; h } =
    for i = x to x + w - 1 do
      for j = y to y + h - 1 do
        write_pixel b i j color
      done
    done
  in
  List.iter write_rectangle l

type conn = {
  buffer : Bytes.t;
  sock : Lwt_unix.file_descr;
}

let connect ?(url = "151.217.21.22") ?(port = 1337) () =
  let sock = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
  let%lwt () = Lwt_unix.connect sock Unix.(ADDR_INET (inet_addr_of_string url, port)) in
  let buffer = mk_buffer () in
  Lwt.return { buffer; sock }

let send_image conn (img: image) =
  Bytes.fill conn.buffer 0 image_buffer_size (Char.chr 0);
  img conn.buffer;
  let%lwt _ = Lwt_unix.send conn.sock conn.buffer 0 image_buffer_size [] in
  Lwt.return ()

let disconnect conn =
  Lwt_unix.shutdown conn.sock Unix.SHUTDOWN_ALL
