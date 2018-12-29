open Matelightlib

let () = Random.self_init ()

type state = {
  ball_x : float;
  ball_y : float;
  ball_vr : float;
  ball_vtheta : float;

  p1 : int;
  p2 : int;

  (* oldest first *)
  trail : (float * float) list;
}

let ncol = float_of_int Screen.nb_columns
let nlines = float_of_int Screen.nb_lines

let ball_speed = 0.5
let player_width = 2.
let player_height = 6
let trail_length = 8
let trail_colors = [
  Screen.rgb ~r:255 ~g:0 ~b:0;
  Screen.rgb ~r:255 ~g:0 ~b:0;
  Screen.rgb ~r:255 ~g:144 ~b:0;
  Screen.rgb ~r:255 ~g:144 ~b:0;
  Screen.rgb ~r:255 ~g:230 ~b:0;
  Screen.rgb ~r:255 ~g:230 ~b:0;
  Screen.rgb ~r:255 ~g:230 ~b:130;
  Screen.rgb ~r:255 ~g:230 ~b:130;
]

let pi = 3.1415926535

let random_vtheta () =
  Random.float (2./.8.) +. (1./.8.)
  +. (float_of_int (Random.int 3)) *. pi

let st0 () = {
  ball_x = ncol /. 2.;
  ball_y = nlines /. 2.;
  ball_vr = ball_speed;
  ball_vtheta = random_vtheta ();
  p1 = (Screen.nb_lines - player_height) / 2;
  p2 = (Screen.nb_lines - player_height) / 2;
  trail = [];
}

let upd_trail x y trail =
  if List.length trail = trail_length then
    List.tl trail @ [(x, y)]
  else
    trail @ [(x, y)]

let game_step st =
  let st = { st with trail = upd_trail st.ball_x st.ball_y st.trail } in
  let x' = st.ball_x +. st.ball_vr *. cos st.ball_vtheta in
  let y' = st.ball_y +. st.ball_vr *. sin st.ball_vtheta in

  let st =
    if y' < 0. then
      (* bounce top *)
      { st with ball_y = -. y';
                ball_vtheta = pi -. st.ball_vtheta;
                ball_vr = -. st.ball_vr; }
    else if y' >= nlines then
      (* bounce bottom *)
      { st with ball_y = 2. *. nlines -. y' -. 1.;
                ball_vtheta = pi -. st.ball_vtheta;
                ball_vr = -. st.ball_vr; }
    else
      (* continue *)
      { st with ball_y = y' }
  in

  let deviation theta diff =
    let diff_max = float_of_int player_height in
    let diff_ratio = diff /. diff_max in
    if theta > 0. then
      min (theta +. diff_ratio *. pi /. 8.) (2. *. pi /. 6.)
    else
      max (theta -. diff_ratio *. pi /. 8.) (- 2. *. pi /. 6.)
  in

  let st, reset =
    if x' < player_width &&
       float_of_int st.p1 <= st.ball_y &&
       st.ball_y <= float_of_int (st.p1 + player_height)
       (* left player collision *)
    then
      (* bounce *)
      { st with ball_x = 2. *. player_width -. x';
                ball_vr = -. st.ball_vr;
                ball_vtheta =
                  deviation (-. st.ball_vtheta)
                    (float_of_int st.p1 -. st.ball_y);
      }, false
    else if x' < 0. then
      (* P1 lost; reset the ball *)
      st0 (), true
    else if x' >= ncol -. player_width &&
            float_of_int st.p2 <= st.ball_y &&
            st.ball_y <= float_of_int (st.p2 + player_height)
            (* right wall collision *)
    then
      (* bounce *)
      { st with ball_x = 2. *. (ncol -. player_width) -. x' -. 1.;
                ball_vr = -. st.ball_vr;
                ball_vtheta =
                  deviation (-. st.ball_vtheta)
                    (float_of_int st.p2 -. st.ball_y);
      }, false
    else if x' >= ncol then
      (* P2 lost; reset the ball *)
      st0 (), true
    else
      (* continue *)
      { st with ball_x = x' }, false
  in
  let%lwt () = if reset then Lwt_unix.sleep 1. else Lwt.return () in
  Lwt.return { st with ball_vr = st.ball_vr *. 1.001; }

(* Ball with manual "anti-aliasing"
  . X .
  X X X
  . X .
*)
let ball color x y : Screen.image =
  let r, g, b = Screen.get_rgb color in
  let shade_level = 3 in
  let shade_color =
    Screen.rgb ~r:(r / shade_level) ~g:(g / shade_level) ~b:(b / shade_level)
  in
  let px color x y = Screen.{ color; x; y; w = 1; h = 1 } in

  Screen.of_rectangles [
    { color; x; y = y - 1; w = 1; h = 3 };
    { color; x = x - 1; y; w = 3; h = 1 };

    (* shade *)
    px shade_color (x - 1) (y - 1);
    px shade_color (x - 1) (y + 1);
    px shade_color (x + 1) (y - 1);
    px shade_color (x + 1) (y + 1);
  ]

let trail st : Screen.image =
  let rec list_map2 f l1 l2 =
    match l1, l2 with
    | x :: xs, y :: ys ->
      f x y :: list_map2 f xs ys
    | _, _ ->
      []
  in
  list_map2 (fun (x, y) color ->
    let r, g, b = Screen.get_rgb color in
    let color = Screen.rgb ~r:(r / 2) ~g:(g / 2) ~b:(b / 2) in
    ball color (int_of_float x) (int_of_float y)
  ) st.trail trail_colors
  |> Screen.superpose

let players p1 p2 : Screen.image =
  Screen.of_rectangles [
    { color = Screen.rgb ~r:255 ~g:255 ~b:0;
      x = 0; y = p1; w = 2; h = player_height };
    { color = Screen.rgb ~r:0 ~g:255 ~b:255;
      x = Screen.nb_columns - 2; y = p2; w = 2; h = player_height };
  ]

let draw conn st =
  Screen.superpose [
    trail st;
    ball 0xFFFFFF (int_of_float st.ball_x) (int_of_float st.ball_y);
    players st.p1 st.p2;
  ] |> Screen.send_image conn

let apply_keys events st =
  let p1_up st = { st with p1 = max 0 (st.p1 - 1) } in
  let p1_down st = { st with p1 = min (Screen.nb_lines - player_height) (st.p1 + 1) } in
  let p2_up st = { st with p2 = max 0 (st.p2 - 1) } in
  let p2_down st = { st with p2 = min (Screen.nb_lines - player_height) (st.p2 + 1) } in
  Lwt_stream.get_available events |> List.fold_left (fun st ev ->
    match ev with
    | `Up1 -> p1_up st
    | `Down1 -> p1_down st
    | `Up2 -> p2_up st
    | `Down2 -> p2_down st
  ) st

let rec game_loop conn events st =
  let%lwt () = draw conn st in
  let%lwt () = Lwt_unix.sleep 0.025 in
  let st' = apply_keys events st in
  let%lwt st' = game_step st' in
  game_loop conn events st'

let rec player_loop sock buf send_up send_down =
  let%lwt (len, _) = Lwt_unix.recvfrom sock buf 0 1 [] in
  if len > 0 then begin
    match Bytes.get buf 0 with
    | 'u' -> send_up ()
    | 'd' -> send_down ()
    | _ -> ()
  end;
  player_loop sock buf send_up send_down

let port1 = ref 4242
let port2 = ref 4243

let main () =
  let%lwt conn = Screen.connect () in
  let events, send_event = Lwt_stream.create () in
  let sock1 = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
  let sock2 = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
  let%lwt () = Lwt_unix.bind sock1
      Unix.(ADDR_INET (inet_addr_of_string "0.0.0.0", !port1)) in
  let%lwt () = Lwt_unix.bind sock2
      Unix.(ADDR_INET (inet_addr_of_string "0.0.0.0", !port2)) in
  let p1_loop =
    player_loop sock1 (Bytes.create 1)
      (fun () -> send_event (Some `Up1))
      (fun () -> send_event (Some `Down1)) in
  let p2_loop =
    player_loop sock2 (Bytes.create 1)
      (fun () -> send_event (Some `Up2))
      (fun () -> send_event (Some `Down2)) in
  Lwt.join [game_loop conn events (st0 ()); p1_loop; p2_loop]

let () =
  let usage = "./pong --port1 PORT1 --port2 PORT2" in
  let bad_args _ = Printf.printf "Usage: %s\n" usage; exit 1 in
  Arg.parse [
    "--port1", Arg.Set_int port1, " Port for player 1 (default: 4242)";
    "--port2", Arg.Set_int port2, " Port for player 2 (default: 4243)";
  ] bad_args usage;
  Lwt_main.run (main ())
