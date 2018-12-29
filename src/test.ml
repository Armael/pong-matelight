open Matelightlib

let main =
  let%lwt conn = Screen.connect () in
  let img = Screen.[
    { color = rgb ~r:255 ~g:0 ~b:0;
      x = 0; y = 0;
      w = nb_columns; h = nb_lines }
  ] |> Screen.of_rectangles
  in
  let rec loop () =
    let%lwt () = Screen.send_image conn img in
    let%lwt () = Lwt_unix.sleep 0.33 in
    loop () in
  loop ()

let () = Lwt_main.run main
