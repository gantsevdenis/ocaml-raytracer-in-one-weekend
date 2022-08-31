open Gg

type ray_t = {o: Gg.V3.t; dir: Gg.V3.t}

type color_t = V3.t

(* type point_t = V3.t *)

let make_ray o (dir : Gg.V3.t) = {o; dir}

let () =
  let module V3 = Gg.V3 in
  Dolog.Log.set_output stderr ;
  Dolog.Log.(set_log_level INFO) ;
  let aspect_ratio = 16.0 /. 9.0 in
  let w, h = (400, Float.trunc (400. /. aspect_ratio) |> Float.to_int) in
  let viewport_h = 2. in
  let viewport_w = aspect_ratio *. viewport_h in
  let focal_len = 1. in
  let o = V3.zero in
  let horizontal = V3.v viewport_w 0. 0. in
  let vertical = V3.v 0. viewport_h 0. in
  let neg_half_v = V3.(smul (-0.5) vertical) in
  let neg_half_h = V3.(smul (-0.5) horizontal) in
  let v_focal_len = V3.v 0. 0. (-.focal_len) in
  let low_left = V3.(add o (add neg_half_h (add neg_half_v v_focal_len))) in
  let write_color (c : color_t) : unit =
    let r = Float.to_int (255.999 *. V3.x c) in
    let g = Float.to_int (255.999 *. V3.y c) in
    let b = Float.to_int (255.999 *. V3.z c) in
    Printf.printf "%i %i %i\n" r g b
  in
  let ray_color (r : ray_t) : color_t =
    let unit_dir = V3.unit r.dir in
    let t = 0.5 *. (V3.y unit_dir +. 1.) in
    let t_1 = 1. -. t in
    let light_blue = V3.v 0.5 0.7 1. in
    let one = V3.v 1. 1. 1. in
    V3.add (V3.smul t_1 one) (V3.smul t light_blue)
  in
  let write_ppm ~w ~h =
    let j = ref (h - 1) in
    let i = ref 0 in
    Printf.printf "P3\n%i %i\n255\n" w h ;
    while !j >= 0 do
      Dolog.Log.debug "Scanlines remaining %i" !j ;
      let v = Float.of_int !j /. Float.of_int (h - 1) in
      while !i < w do
        let u = Float.of_int !i /. Float.of_int (w - 1) in
        let uh = V3.smul u horizontal in
        let uv = V3.smul v vertical in
        let uv_plus_uh = V3.add uh uv in
        let low_left_minus_o = V3.add low_left (V3.smul (-1.) o) in
        let ray = make_ray o (V3.add low_left_minus_o uv_plus_uh) in
        let c = ray_color ray in
        write_color c ;
        i := !i + 1
      done ;
      i := 0 ;
      j := !j - 1
    done
  in
  write_ppm ~w ~h
