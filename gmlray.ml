open Printf
open Random

let test_run = false

(* paramètre de passage du monde virtuel au monde réel *)
let npixx = 95
let npixy = 63
let distance_eye_screen_in_pixels = float_of_int npixy *. 1.4


(* définition et parcours des pixels *)
type brightness = White | Black

let xf = float_of_int npixx and yf = float_of_int npixy

let output_image_from_pixel_bw_values filename pixelcoords_to_brightness =
  let oc = open_out filename in
    fprintf oc "P1\n# CREATOR: glmray (c) Stephane Gourichon\n%d %d\n" npixx npixy;
    for y = npixy - 1 downto 0
    do
      for x = 0 to npixx -1
      do
	(*printf "x=%d y=%d " x y;*)
	fprintf oc "%c" (
          match pixelcoords_to_brightness x y with
              White -> '0';
            | Black -> '1';
	);
      done
    done;
    close_out oc

(* exemples d'utilisations jusque là : 2D 1bit *)

let xor a b =
  if a then not b else b;;

let _ =
  if (test_run) then
    begin
      output_image_from_pixel_bw_values "checkerboard_small.pbm"
	(fun x y -> if (((x+y) mod 2)=1) then Black else White);

    let taille = 8 in
    let pulsewidth = taille / 2 in
    let checkerboard_big x y =
      if (
	xor ((x mod taille) >= pulsewidth)
	  ((y mod taille) >= pulsewidth)
      )
      then Black else White in
      output_image_from_pixel_bw_values "checkerboard_big.pbm"
	checkerboard_big;
    end

(* on va gérer les niveaux de gris *)

let clamp_to_byte float =
  let byte_value = int_of_float (256. *. float) in
    if (byte_value<0) then 0 else
    if (byte_value>255) then 255 else
      byte_value;;

let output_image_from_pixel_grey_values filename pixelcoords_to_grey_value =
  let oc = open_out filename in
    fprintf oc "P2\n# CREATOR: glmray (c) Stephane Gourichon\n%d %d\n255\n" npixx npixy;
    for y = npixy - 1 downto 0
    do
      for x = 0 to npixx -1
      do
	(*printf "x=%d y=%d " x y;*)
	fprintf oc "%d\n" (clamp_to_byte (pixelcoords_to_grey_value x y))
      done
    done;
    close_out oc;;

(* routines passant d'une valeur en niveau de gris à une valeur noir ou blanc *)

(* dither simple *)
Random.init 0;;

let grey_to_brightness_random_dither x y grey_value =
  if (grey_value > Random.float 1.0) then White else Black;;

(* dither avec trame *)

type pattern = {
	       pattern_normalisation : float;
	       pattern_data : int array array };;

let pattern_16 = {
  pattern_normalisation = 32.;
  pattern_data = [|
    [| 01;17;05;21 |];
    [|  25;09;29;13 |];
    [|  07;23;03;19 |];
    [|  31;15;27;11 |]
  |]
};;


let grey_to_brightness_pattern x y grey_value =
  let threshold = float_of_int (pattern_16.pattern_data.(x mod 4).(y mod 4)) in
    if (pattern_16.pattern_normalisation *. grey_value > threshold) then White else Black;;

(* routine de confort pour sortir toutes les images à la fois.
Oui, l'image est recalculée plusieurs fois.
Oui on pourrait faire un buffer de flottants.
*)

let output_several_image_from_pixel_grey_values filenamestub pixelcoords_to_grey_value =
  output_image_from_pixel_grey_values (filenamestub^".grey.pgm") pixelcoords_to_grey_value ;
  output_image_from_pixel_bw_values (filenamestub^".dithered.pbm")
  (fun x y -> grey_to_brightness_random_dither x y
    (pixelcoords_to_grey_value x y));
  output_image_from_pixel_bw_values (filenamestub^".pattern.pbm")
  (fun x y -> grey_to_brightness_pattern x y
    (pixelcoords_to_grey_value x y));;


(* définitions pour le virtuel 3D *)
type vect3d = { x:float; y:float; z:float };;

(* fonctions annexes *)
let carre x = x *. x ;;
let norme2 v = carre v.x +. carre v.y +. carre v.z;;
let norme v = sqrt (norme2 v);;
let dot_product v1 v2 = v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z ;;

let (--) v1 v2 = { x=v1.x-.v2.x;  y=v1.y-.v2.y;  z=v1.z-.v2.z; };;
let (//) v l = { x=v.x/.l;  y=v.y/.l;  z=v.z/.l; };;

let vecteur_a_vers_b a b =
  b -- a;;

(*let string_of_vect3d v = "{x="^string_of_float v.x^",y="^string_of_float v.y^",z="^string_of_float v.z^"}";;*)
let string_of_vect3d v = sprintf "{%.2f,%.2f,%.2f}" v.x v.y v.z;;

(* passage des pixels au monde virtuel 3d : à partir des coordonnées
   du pixel on calcule le vecteur regard en 3D *)
let gazevector_of_pixcoords x y =
  {x= (float_of_int ((x*2)-npixx)) /. distance_eye_screen_in_pixels;
   y= (float_of_int ((y*2)-npixy)) /. distance_eye_screen_in_pixels;
   z= -1.};;

(* Ok on a un vecteur regard. Maintenant il faut calculer ce qu'il
   touche. *)

(* Une fontion d'intersection prend un vecteur regard et renvoie le
   point touché et la normal en ce point. *)
type point_avec_normale = { position : vect3d ; normale_normee : vect3d };;

type intersection = No_intersect
		    | Intersect of point_avec_normale;;

type fonction_intersection = vect3d -> intersection;;

(* A partir du point d'intersection et de la normale on peut calculer
   l'éclairement. *)
type lampe_ponctuelle = { position_lampe : vect3d ; force_lampe : float };;

let lampe = { position_lampe = {x= -1.;y= 1.;z= 0.} ; force_lampe = 5. };;

let sky_color = 1.;;

let intersection_to_greyvalue lieu_d_intersection =
  match lieu_d_intersection with
      No_intersect -> sky_color |
      Intersect point_avec_normale ->
	let lamp_to_surface = lampe.position_lampe -- point_avec_normale.position in
	let dist2 = norme2 ( lamp_to_surface ) in
	let dist = sqrt (dist2) in
	let local_illumination = lampe.force_lampe *. (dot_product lamp_to_surface point_avec_normale.normale_normee) /. dist /. dist2  in
	  local_illumination;;

(* on se donne une fonction qui partant d'un vecteur donnant la
   direction du regard (gaze vector) donne une intersection *)

(* ok maintenant le plan horizontal. On prend des coordonnées et on renvoie le vecteur intersection et le vecteur normal.
   x=x0+vx*t
   y=y0+vy*t
   z=z0+vz*t

   y=y_plane

   t=(yplane-y0)/vy
*)

let intersect_horizontal_plane v =
  let y_plane = -1. in
    if v.y<0. then
      let ti = y_plane /. v.y in
	Intersect {position={x=v.x *. ti; y=y_plane; z=v.z *. ti};
	 normale_normee={x=0.;y=1.;z=0.}}
    else
      No_intersect;;


type sphere = { sph_pos : vect3d ; sph_radius : float };;
let ma_sphere = { sph_pos = { x=0.;y=0.;z= -2. } ; sph_radius = 1.0 ; };;


(*
On calcule l'intersection entre :

-la sphère de centre sph_pos et de rayon sph_radius

   (x-sph_x)²+(y-sph_y)²+(z-sph_z)²=sph_radius²

-la droite paramétrée de centre l'origine et de vecteur v

   x=x0+vx*t
   y=y0+vy*t
   z=z0+vz*t

On substitue et développe.

((x0-sph_x)+vx*t)²+((y0-sph_y)+vy*t)²+((z0-sph_z)+vz*t)² - sph_radius² = 0
Terme en t² : a = norme2(v)
Terme en t  : b = -2*scalaire(vcentres,v) où vcentres = sph_pos - obs_pos
Terme en 1  : c = norme2(vcentres) - sph_radius²

En d'autres termes :
sphere : norme2(sph_pos - intersect) = sph_radius²
droite : intersect = obs_pos + v * t

norme2(sph_pos - obs_pos - v * t) = sph_radius²
on fait apparaître vcentres :
norme2(vcentres - v * t) - sph_radius² = 0
ce qui donne bien la même chose.

Plutôt que de résoudre cette équation on va en résoudre une autre qui a les mêmes solutions, obtenue à partir de la première en  divisant tout par 2.
a' = a/2 = norme2(v)/2
b' = b/2 = - scalaire(vcentres,v)
c' = c/2 = (norme2(vcentres) - sph_radius²)/2

discriminant' = scalaire(vcentres,v)^2 - norme2(v) * (norme2(vcentres) - sph_radius²)
si discriminant'<0 pas d'intersection
si discriminant'>=0 deux solutions.

t=(-b'+-sqrt(discriminant'))/2a'

On prend celle de plus petit t.
a étant positif, c'est la solution avec signe moins qui donne le plus petit t.

    scalaire(vcentres,v) - sqrt(discriminant')
t = ------------------------------------------
                   norme2(v)

   une fois obtenue la position de l'intersection on veut un vecteur
   normal à la sphère, vers l'extérieur, normé.

   La réponse est simple :
   ( pos_intersect - sph_pos ) divisé par sa norme.

*)

let intersect_sphere v =
  let vcentres = ma_sphere.sph_pos in
    (* si obs_pos non nul vcentres devient vecteur_a_vers_b obs_pos sph_pos *)
  let svcv = dot_product vcentres v
  and n2v = norme2 v
  and c = norme2 vcentres -. carre ma_sphere.sph_radius in
  let discriminantp = carre svcv -. n2v *. c in
    (*printf "v %s svcv %.6f n2v %.6f d %.6f\n" (string_of_vect3d v) svcv n2v discriminantp;*)
    if (discriminantp > 0.)
    then
      let ti = ( (dot_product vcentres v) -. sqrt(discriminantp) ) /. n2v in
      let pos_intersect = {x=v.x *. ti; y=v.y *. ti; z=v.z *. ti} in
      let v_normal_non_norme = pos_intersect -- ma_sphere.sph_pos in
	Intersect {
	  position = pos_intersect;
	  normale_normee = v_normal_non_norme // norme v_normal_non_norme
	}
    else
      No_intersect;;


(* la valeur d'un pixel dépend de la position de l'intersection et de la normale *)

(*output_image_from_3d_intersect_function (intersection_to_greyvalue intersect_horizontal_plane);;*)

(* Pour le côté pratique on masque l'aspect 2D.  On peut définir une
   fonction adaptatrice qu'il faudra "brancher" sur la boucle
   principale à base de pixels ou bien définir une nouvelle boucle
   principale qu'on appliquera directement à une fonction 3D. *)
(* let output_image_from_3d_intersect_function f = *)
(*   in output_image_from_pixel_bw_values pixelcoords_to_brightness;; *)


(* output_image_from_pixel_bw_values "plan.pbm"
   (fun x y ->
   let gazevector = gazevector_of_pixcoords x y in
   let intersection = intersect_horizontal_plane gazevector in
   let greyvalue = intersection_to_greyvalue intersection in
   grey_to_brightness_pattern x y greyvalue);;

   output_image_from_pixel_bw_values "plan02.pbm"
   (fun x y -> grey_to_brightness_pattern x y
   (intersection_to_greyvalue
   (intersect_horizontal_plane
   (gazevector_of_pixcoords x y))));; *)

if test_run then
output_several_image_from_pixel_grey_values "plan"
  (fun x y ->
    (intersection_to_greyvalue
	(intersect_horizontal_plane
	    (gazevector_of_pixcoords x y))));;



output_several_image_from_pixel_grey_values "sphere"
  (fun x y ->
    let gazevector = gazevector_of_pixcoords x y in
    let intersection =
      match intersect_sphere gazevector  with
	  Intersect intersection_sphere as valeur -> valeur
	| No_intersect ->
	    intersect_horizontal_plane gazevector in
     intersection_to_greyvalue intersection);;
