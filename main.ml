open Gg

type color_t = V3.t

type point_t = V3.t

type ray_t = {o: point_t; dir: point_t}

let ray_make o dir = {o; dir}

let ray_at r t = V3.add r.o (V3.smul t r.dir)

type sphere_t = {center: point_t; radius: float}

let sphere_make center radius = {center; radius}

type hit_record = {p: point_t; normal: point_t; t: float; is_front: bool}

type camera_t = {o: point_t; low_left: point_t; hor: point_t; ver: point_t}

let camera_make () =
  let aspect_ratio = 16.0 /. 9.0 in
  (* let w, h = (400, Float.trunc (400. /. aspect_ratio) |> Float.to_int) in *)
  let viewport_h = 2. in
  let viewport_w = aspect_ratio *. viewport_h in
  let focal_len = 1. in
  let o = V3.zero in
  let hor = V3.v viewport_w 0. 0. in
  let ver = V3.v 0. viewport_h 0. in
  let neg_half_v = V3.(smul (-0.5) ver) in
  let neg_half_h = V3.(smul (-0.5) hor) in
  let v_focal_len = V3.v 0. 0. (-.focal_len) in
  let low_left = V3.(add o (add neg_half_h (add neg_half_v v_focal_len))) in
  {o; low_left; hor; ver}

let camera_get_ray self u v =
  let sum_basis = V3.add (V3.smul u self.hor) (V3.smul v self.ver) in
  let sum_basis_o = V3.sub sum_basis self.o in
  ray_make self.o (V3.add self.low_left sum_basis_o)

(* let radians_of_degree deg = deg *. Float.pi /. 180. *)
let clamp (x : float) min_ max_ = max (min x max_) min_

let sphere_outward_normal self p =
  V3.smul (1. /. self.radius) (V3.sub p self.center)

let hit_record_sphere r root sphere =
  let p = ray_at r root in
  let outward_normal = sphere_outward_normal sphere p in
  let is_front = V3.dot r.dir outward_normal < 0. in
  let normal =
    if is_front then outward_normal else V3.smul (-1.) outward_normal
  in
  {p; t= 0.; normal; is_front}

let () =
  let module V3 = Gg.V3 in
  Dolog.Log.set_output stderr ;
  Dolog.Log.(set_log_level INFO) ;
  let aspect_ratio = 16.0 /. 9.0 in
  let w, h = (400, Float.trunc (400. /. aspect_ratio) |> Float.to_int) in
  let n_samples = 32 in
  let cam = camera_make () in
  let world =
    [ sphere_make (V3.v 0. 0. (-1.)) 0.5
    ; sphere_make (V3.v 0. (-100.5) (-1.)) 100. ]
  in
  let write_color (c : color_t) (n_samples : int) : unit =
    let scale = 1. /. (n_samples |> Float.of_int) in
    let r = clamp (V3.x c *. scale) 0. 0.999 *. 256. |> Float.to_int in
    let g = clamp (V3.y c *. scale) 0. 0.999 *. 256. |> Float.to_int in
    let b = clamp (V3.z c *. scale) 0. 0.999 *. 256. |> Float.to_int in
    Printf.printf "%i %i %i\n" r g b
  in
  let hit_sphere self (r : ray_t) t_min t_max =
    let oc = V3.sub r.o self.center in
    let a = V3.norm2 r.dir in
    let half_b = V3.dot oc r.dir in
    let c = V3.norm2 oc -. (self.radius *. self.radius) in
    let discriminant = (half_b *. half_b) -. (a *. c) in
    if discriminant < 0. then None
    else
      let sqrd = sqrt discriminant in
      let root_1 = (-.half_b -. sqrd) /. a in
      let root_2 = (-.half_b +. sqrd) /. a in
      let root = if root_1 < t_min || t_max < root_1 then root_2 else root_1 in
      if root < t_min || t_max < root then None
      else Some (hit_record_sphere r root self)
  in
  let world_hit world (r : ray_t) ~t_min ~t_max =
    let rec _loop l ~(closest : float) out_rec =
      match l with
      | x :: tail -> (
        match hit_sphere x r t_min closest with
        | Some a as temp_rec ->
            _loop tail ~closest:a.t temp_rec
        | None ->
            _loop tail ~closest out_rec )
      | [] ->
          out_rec
    in
    _loop world ~closest:t_max None
  in
  let ray_color (r : ray_t) world : color_t =
    match world_hit world r ~t_min:0. ~t_max:infinity with
    | Some rr ->
        (* Dolog.Log.warn "hit!" ; *)
        V3.smul 0.8 (V3.add rr.normal (V3.v 1. 1. 1.))
    | None ->
        let unit_dir = V3.unit r.dir in
        let t = 0.5 *. (V3.y unit_dir +. 1.) in
        V3.add (V3.smul (1. -. t) (V3.v 1. 1. 1.)) (V3.smul t (V3.v 0.5 0.5 1.))
  in
  let random_double () = Random.float 1. in
  let write_ppm ~w ~h =
    let j = ref (h - 1) in
    let i = ref 0 in
    Printf.printf "P3\n%i %i\n255\n" w h ;
    while !j >= 0 do
      Dolog.Log.debug "Scanlines remaining %i" !j ;
      while !i < w do
        let c = ref V3.zero in
        for _ = 0 to n_samples - 1 do
          let u =
            (Float.of_int !i +. random_double ()) /. Float.of_int (w - 1)
          in
          let v =
            (Float.of_int !j +. random_double ()) /. Float.of_int (h - 1)
          in
          let ray = camera_get_ray cam u v in
          c := V3.add !c (ray_color ray world)
        done ;
        (* Dolog.Log.warn "color %f %f %f" (V3.x !c) (V3.y !c) (V3.z !c) ; *)
        write_color !c n_samples ;
        i := !i + 1
      done ;
      i := 0 ;
      j := !j - 1
    done
  in
  write_ppm ~w ~h
