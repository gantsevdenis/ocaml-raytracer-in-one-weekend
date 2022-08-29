open Gg

(*type ray_t = {o: Gg.V3.t; dir: Gg.V3.t}

  let make_ray o dir = {o; dir}

  let write_ppm ~w ~h =
    let j = ref (h - 1) in
    let i = ref 0 in
    Printf.printf "P3\n%i %i\n255\n" w h ;
    while !j >= 0 do
      while !i < w do
        let r = Float.of_int !i /. Float.of_int (w - 1) in
        let g = Float.of_int !j /. Float.of_int (h - 1) in
        let b = 0.25 in
        let r = Float.to_int (255.999 *. r) in
        let g = Float.to_int (255.999 *. g) in
        let b = Float.to_int (255.999 *. b) in
        Printf.printf "%i %i %i\n" r g b ;
        i := !i + 1
      done ;
      j := !j - 1
    done *)

let () =
  let module V3 = Gg.V3 in
  let aspect_ratio = 16.0 /. 9.0 in
  let w, h = (400, Float.trunc (400. /. aspect_ratio) |> Float.to_int) in
  (*let viewport_h = 2. in
    let viewport_w = aspect_ratio *. viewport_h in
    let focal_len = 1. in
    let o = V3.zero in
    let horizontal = V3.v 0. viewport_h 0. in
    let vertical = V3.v 0. viewport_h 0. in
    let old_minus = ( -. ) in
    let old_mult = ( *. ) in
     let old_plus = ( +. ) in *)
  (* let ( -. ) = V3.sub in
     let ( *. ) = V3.smul in *)
  (* let ( +. ) a b = a -. (-1. *. b) in
     let low_left =
       o -. (0.5 *. horizontal) -. (0.5 *. vertical) -. V3.v 0. 0. focal_len in *)

  (* let dir = make_ray o (low_left -. o +. horizontal +. vertical) in *)
  (* let ( -. ) = old_minus in
     let ( *. ) = old_mult in
     let ( +. ) = old_plus in *)
  Dolog.Log.set_output stderr ;
  Dolog.Log.(set_log_level INFO) ;
  let write_ppm ~w ~h =
    let j = ref (h - 1) in
    let i = ref 0 in
    Printf.printf "P3\n%i %i\n255\n" w h ;
    while !j >= 0 do
      Dolog.Log.info "Scanlines remaining %i" !j ;
      while !i < w do
        let r = Float.of_int !i /. Float.of_int (w - 1) in
        let g = Float.of_int !j /. Float.of_int (h - 1) in
        let b = 0.25 in
        let r = Float.to_int (255.999 *. r) in
        let g = Float.to_int (255.999 *. g) in
        let b = Float.to_int (255.999 *. b) in
        Printf.printf "%i %i %i\n" r g b ;
        i := !i + 1
      done ;
      i := 0;
      j := !j - 1
    done
  in
  write_ppm ~w ~h
