open Gg

type color_t = V3.t

type point_t = V3.t

type ray_t = {o: point_t; dir: point_t}

let ray_make o dir = {o; dir}

let ray_at r t = V3.add r.o (V3.smul t r.dir)

type camera_t =
  { o: point_t
  ; low_left: point_t
  ; hor: point_t
  ; ver: point_t
  ; aspect_ratio: float
  ; lens_radius: float
  ; u: point_t
  ; v: point_t
  ; w: point_t }

let random_float min_ max_ = min_ +. ((max_ -. min_) *. Random.float 0.99999)

let random_float_1_1 () = random_float (-1.) 1.

let random_vec3 min_ max_ =
  let a = random_float min_ max_ in
  let b = random_float min_ max_ in
  let c = random_float min_ max_ in
  V3.v a b c

let random_vec3_in_unit_sphere () =
  let rec _loop vec =
    if V3.norm2 vec >= 1. then _loop (random_vec3 (-1.) 1.) else vec
  in
  _loop (random_vec3 (-1.) 1.)

let random_vec3_unit_vec () = V3.unit (random_vec3_in_unit_sphere ())

let random_vec3_in_hemisphere normal =
  let in_unit_sphere = random_vec3_in_unit_sphere () in
  if V3.dot in_unit_sphere normal > 0. then
    (* same hemisphere as the normal *)
    normal
  else V3.smul (-1.) normal

let random_in_unit_disk () =
  let rec _loop vec =
    if V3.norm2 vec >= 1. then
      _loop (V3.v (random_float_1_1 ()) (random_float_1_1 ()) 0.)
    else vec
  in
  _loop (V3.v (random_float_1_1 ()) (random_float_1_1 ()) 0.)

type diffuse_material = {albedo: color_t}

type metal_material = {albedo: color_t; fuzz: float}

type dielectric_material = {ir: float}

let near_zero v =
  let s = 1e-8 in
  let fabs = abs_float in
  fabs (V3.x v) < s && fabs (V3.y v) < s && fabs (V3.z v) < s

let reflect v n = V3.sub v (V3.smul (2. *. V3.dot v n) n)

let refract uv n etai_over_etat =
  let cos_theta = min (V3.dot (V3.smul (-1.) uv) n) 1. in
  let r_out_perp = V3.smul etai_over_etat (V3.add uv (V3.smul cos_theta n)) in
  let r_out_parall =
    V3.smul (-.sqrt (abs_float (1. -. V3.norm2 r_out_perp))) n
  in
  V3.add r_out_parall r_out_perp

let diffuse_scatter (self : diffuse_material) ~p ~normal =
  let scatter_direction = V3.add normal (random_vec3_unit_vec ()) in
  let scatter_direction =
    (* Catch degenerate scatter direction *)
    if near_zero scatter_direction then normal else scatter_direction
  in
  let scaterred = ray_make p scatter_direction in
  Some (scaterred, self.albedo)

let metal_scatter (self : metal_material) ray_in ~p ~normal =
  let reflected = reflect (V3.unit ray_in.dir) normal in
  let scattered =
    ray_make p
      (V3.add reflected (V3.smul self.fuzz (random_vec3_in_unit_sphere ())))
  in
  if V3.dot scattered.dir normal > 0. then Some (scattered, self.albedo)
  else None

let reflectance cosine ref_idx =
  (* Use Schlick's approximation for reflectance *)
  let r0 = (1. -. ref_idx) /. (1. +. ref_idx) in
  let r0 = r0 *. r0 in
  r0 +. ((1. -. r0) *. Float.pow (1. -. cosine) 5.)

let dielectric_scatter self ray_in front ~p ~normal =
  (* glass surface absorbs nothing *)
  let c = V3.v 1. 1. 1. in
  let refraction_ratio = if front then 1. /. self.ir else self.ir in
  let unit_dir = V3.unit ray_in.dir in
  let cos_theta = min (V3.dot (V3.smul (-1.) unit_dir) normal) 1. in
  let sin_theta = sqrt (1. -. (cos_theta *. cos_theta)) in
  let cannot_refract = refraction_ratio *. sin_theta > 1.0 in
  let direction =
    if
      cannot_refract
      || reflectance cos_theta refraction_ratio > random_float_1_1 ()
    then reflect unit_dir normal
    else refract unit_dir normal refraction_ratio
  in
  let scattered = ray_make p direction in
  Some (scattered, c)

type material_t =
  | Diffuse of diffuse_material
  | Metallic of metal_material
  | Dielectric of dielectric_material

let scatter m ray front ~p ~normal =
  match m with
  | Diffuse m' ->
      diffuse_scatter m' ~p ~normal
  | Metallic m' ->
      metal_scatter m' ray ~p ~normal
  | Dielectric m' ->
      dielectric_scatter m' ray front ~p ~normal

type sphere_t = {center: point_t; radius: float; mat: material_t}

let sphere_make center radius mat = {center; radius; mat}

let radians_of_degree deg = deg *. Float.pi /. 180.

let camera_make ~from ~at ~up vfov ~aspect_ratio ~aperture ~focus_dist =
  let theta = radians_of_degree vfov in
  let h = tan (theta /. 2.) in
  let viewport_h = 2. *. h in
  let viewport_w = aspect_ratio *. viewport_h in
  let w = V3.unit (V3.sub from at) in
  let u = V3.unit (V3.cross up w) in
  let v = V3.cross w u in
  let o = from in
  let ( -- ) = V3.sub in
  let ( $ ) = V3.smul in
  let hor = focus_dist *. viewport_w $ u in
  let ver = focus_dist *. viewport_h $ v in
  let low_left = o -- (0.5 $ hor) -- (0.5 $ ver) -- (focus_dist $ w) in
  let lens_radius = aperture /. 2. in
  {o; low_left; hor; ver; aspect_ratio; u; v; w; lens_radius}

let camera_get_ray self s t =
  let ( -- ) = V3.sub in
  let ( ++ ) = V3.add in
  let ( $ ) = V3.smul in
  let rd = self.lens_radius $ random_in_unit_disk () in
  let offset = V3.((x rd $ self.u) ++ (y rd $ self.v)) in
  let sum_basis = (s $ self.hor) ++ (t $ self.ver) in
  ray_make (self.o ++ offset) (self.low_left ++ sum_basis -- self.o -- offset)

let clamp (x : float) min_ max_ = max (min x max_) min_

let sphere_outward_normal self p =
  V3.smul (1. /. self.radius) (V3.sub p self.center)

type hit_record =
  {p: point_t; normal: point_t; t: float; is_front: bool; mat: material_t}

let hit_record_sphere r root sphere mat =
  let p = ray_at r root in
  let outward_normal = sphere_outward_normal sphere p in
  let is_front = V3.dot r.dir outward_normal < 0. in
  let normal =
    if is_front then outward_normal else V3.smul (-1.) outward_normal
  in
  {p; t= root; normal; is_front; mat}

let () =
  (* make compiler happy *)
  ignore random_vec3_unit_vec ;
  ignore random_vec3_in_hemisphere ;
  let module V3 = Gg.V3 in
  Dolog.Log.set_output stderr ;
  Dolog.Log.(set_log_level INFO) ;
  let mat_ground = Diffuse {albedo= V3.v 0.8 0.8 0.} in
  let mat_center = Dielectric {ir= 1.3} in
  let mat_left = Diffuse {albedo= V3.v 0.1 0.2 0.5} in
  let mat_right = Metallic {albedo= V3.v 0.8 0.6 0.2; fuzz= 0.} in
  let from = V3.v (-2.) 2. 1. in
  let at = V3.v 0. 0. (-1.) in
  let up = V3.v 0. 1. 0. in
  let aperture = 2. in
  let focus_dist = V3.(norm (sub from at)) in
  let aspect_ratio = 16. /. 9. in
  let cam =
    camera_make ~from ~at ~up 45.0 ~aspect_ratio ~aperture ~focus_dist
  in
  let w = 400 in
  let h = Float.trunc (Float.of_int w /. cam.aspect_ratio) |> Float.to_int in
  let n_samples = 8 in
  let world =
    [ sphere_make (V3.v 0. (-100.5) (-1.)) 100. mat_ground
    ; sphere_make (V3.v 0. 0. (-1.)) 0.5 mat_center
    ; sphere_make (V3.v (-1.) 0. (-1.)) 0.5 mat_left
    ; (* make a hollow sphere *)
      sphere_make (V3.v (-1.) 0. (-1.)) (-0.45) mat_left
    ; sphere_make (V3.v 1. 0. (-1.)) 0.5 mat_right ]
  in
  let write_color (c : color_t) (n_samples : int) : unit =
    (* gamma-corerct for gamma=2.0, sqrt *)
    let scale = 1. /. (n_samples |> Float.of_int) in
    let r = clamp (V3.x c *. scale |> sqrt) 0. 0.999 *. 256. |> Float.to_int in
    let g = clamp (V3.y c *. scale |> sqrt) 0. 0.999 *. 256. |> Float.to_int in
    let b = clamp (V3.z c *. scale |> sqrt) 0. 0.999 *. 256. |> Float.to_int in
    Printf.printf "%i %i %i\n" r g b
  in
  let sphere_hit self (r : ray_t) t_min t_max =
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
      else Some (hit_record_sphere r root self self.mat)
  in
  let world_hit world (r : ray_t) ~t_min ~t_max =
    let rec _loop l ~(closest : float) out_rec =
      match l with
      | x :: tail -> (
        match sphere_hit x r t_min closest with
        | Some a as temp_rec ->
            _loop tail ~closest:a.t temp_rec
        | None ->
            _loop tail ~closest out_rec )
      | [] ->
          out_rec
    in
    _loop world ~closest:t_max None
  in
  let max_depth = 32 in
  let rec ray_color (r : ray_t) world depth : color_t =
    if depth <= 0 then V3.v 0.5 0.5 0.5
    else
      match world_hit world r ~t_min:0.001 ~t_max:infinity with
      | Some rr -> (
        (* Dolog.Log.warn "hit!" ; *)
        match scatter rr.mat r rr.is_front ~p:rr.p ~normal:rr.normal with
        | None ->
            V3.zero (* (V3.v 0.5 1. 0.5) *)
        | Some (scattered, albedo) ->
            V3.mul albedo (ray_color scattered world (depth - 1)) )
      | None ->
          let unit_dir = V3.unit r.dir in
          let t = 0.5 *. (V3.y unit_dir +. 1.) in
          V3.add
            (V3.smul (1. -. t) (V3.v 1. 1. 1.))
            (V3.smul t (V3.v 0.5 0.7 1.))
  in
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
            (Float.of_int !i +. random_float (-1.) 1.) /. Float.of_int (w - 1)
          in
          let v =
            (Float.of_int !j +. random_float (-1.) 1.) /. Float.of_int (h - 1)
          in
          let ray = camera_get_ray cam u v in
          c := V3.add !c (ray_color ray world max_depth)
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
