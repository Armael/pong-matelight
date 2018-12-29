let server_ip = ref "127.0.0.1"
let port = ref 4242
let keys = ref "kj"

let get1char () =
  let%lwt termio = Lwt_unix.tcgetattr Lwt_unix.stdin in
  let%lwt () = Lwt_unix.tcsetattr Lwt_unix.stdin Lwt_unix.TCSADRAIN
      { termio with Unix.c_icanon = false } in
  let%lwt res = Lwt_io.read_char Lwt_io.stdin in
  let%lwt () = Lwt_unix.tcsetattr Lwt_unix.stdin Lwt_unix.TCSADRAIN termio in
  Lwt.return res

(* We send 'u' to go up, send 'd' to go down *)
let msg = Bytes.of_string "ud"

let rec loop_keys sock =
  let%lwt key = get1char () in
  let%lwt _ =
    if key = !keys.[0] then Lwt_unix.send sock msg 0 1 [] (* u *)
    else if key = !keys.[1] then Lwt_unix.send sock msg 1 1 [] (* d *)
    else Lwt.return 0
  in
  loop_keys sock

let main () =
  let sock = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
  let%lwt () =
    Lwt_unix.connect sock
      Unix.(ADDR_INET (inet_addr_of_string !server_ip, !port)) in
  loop_keys sock

let () =
  let usage = "./pong_client --ip IP --port PORT --keys KEYS" in
  let bad_args _ = Printf.printf "Usage: %s\n" usage; exit 1 in
  Arg.parse [
    "--ip", Arg.Set_string server_ip, " IP of the Pong server (default: 127.0.0.1)";
    "--port", Arg.Set_int port, " Port to connect to (default: 4242)";
    "--keys", Arg.Set_string keys, " Two characters for the up/down keys (by default: kj)";
  ] bad_args usage;
  if String.length !keys <> 2 then bad_args ();
  Lwt_main.run (main ())
