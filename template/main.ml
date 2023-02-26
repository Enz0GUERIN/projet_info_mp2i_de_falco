open Raylib
(* different types pour le jeu comme les différentes cases du plateau, les caractéristiques du joueur
 ou les caractéristiques de la bombe*)

type tile =  Sol | Brique | Mur_Hor |Mur_Vert |B_explosion |B_power
    |B_bombe_plus

type player = {mutable position : int*int;
  mutable vie : int;
  mutable speed : int;
  mutable repousse_bombe : bool ;
  mutable explosion_brick : bool;
  mutable power_bombe : int;
  mutable nombre_bombe : int
}

type bomb = {mutable position : int*int;
  mutable compteur : int;
  mutable duree_explosion : int;
  mutable explosion_brick : bool}

(* premier aperçu de la carte au début du jeu avec les différentes cases en caractères*)

let map = [|"_______________";
            "|...###.###...|";
            "|.__#_#_#_#__.|";
            "|.####...####.|";
            "|#_#_#_#_#_#_#|";
            "|##..#####..##|";
            "|#_#_#_#_#_#_#|";
            "|.####...####.|";
            "|.__#_#_#_#__.|";
            "|...###.###...|";
            "_______________"|]

let tableau = Array.make_matrix 11 15 Sol

let nb_max_bombe = 5

let largeur_ecran, hauteur_ecran = 1000, 800
let max_hauteur = 11
let max_longueur = 15

(* Variables globales représentants la taille de la fenêtre de jeu et les dimensions du plateau *) 

let tile_to_string t =
     match t with
     |Sol ->"."
     |Brique ->"#"
     |Mur_Hor -> "_"
     |Mur_Vert -> "|"
     |B_explosion -> "E"
     |B_power -> "P"
     |B_bombe_plus -> "+"

let char_to_tile t =
     match t with
     |'.' -> Sol
     |'#' -> Brique
     |'_' -> Mur_Hor
     |'|' -> Mur_Vert
     |_ -> Sol

(* fonctions qui permettent de convertir les cases en caractères et inversement *)

let associe_couleur t =
     match t with
     |B_explosion |B_power |B_bombe_plus -> Color.red
     | _ -> Color.white

let map_tile () =
  for i = 0 to max_hauteur-1 do 
    for j = 0 to max_longueur-1 do
       tableau.(i).(j) <- char_to_tile map.(i).[j]
    done
  done

let remplace_brique_par_sol x y =
    Random.self_init ();
    let r = Random.int 4 in
    if r = 0 then tableau.(y).(x) <- Sol
    else if r = 1 then tableau.(y).(x) <- B_explosion
    else if r = 2 then tableau.(y).(x) <- B_power
    else tableau.(y).(x) <- B_bombe_plus

(* fonction qui permet de modifier la carte au moment où une bombe explose une brique pour la transformer en sol *)


let remplace_autre_par_sol x y =
    tableau.(y).(x) <- Sol

let affiche_bomberman (bomberman : player) =
    let y, x = bomberman.position in
    draw_text "@" (50 + x * 46) (25 + y * 46) 50 Color.green

let affiche_bomberman2 (bomberman : player) =
    let y, x = bomberman.position in
    draw_text "@" (50 + x * 46) (25 + y * 46) 50 Color.purple

let affiche_bomb (bomb : bomb) =
    let y, x = bomb.position in
    draw_text "0" (50 + x * 46) (25 + y * 46) 50 Color.white

let affiche_explosion x y (p1 : player) (p2 : player) = 
    let y1, x1 = p1.position in
    let y2, x2 = p2.position in
    draw_text "*" (50 + x * 46) (25 + y * 46) 50 Color.white;
    if (x1 = x) && (y1 = y) then p1.vie <- 0
    else if (x2 = x) && (y2 = y) then p2.vie <- 0

(*Fonctions qui permet l'affichage des joueurs et des actions des bombes: Problème l'explosion de la bombe s'affiche que sur une frame*)

let flagb = ref true

let flagh = ref true

let flagg = ref true

let flagd = ref true

let champ_explosion_centre (bomb : bomb) (p1 : player) (p2 : player) =
    let y, x = bomb.position in
    affiche_explosion x y p1 p2

let champ_explosion_bas (bomb : bomb) (p1 : player) (p2 : player) = 
    let y, x = bomb.position in
    let p = p1.power_bombe in 
    let nb_passage_autorise = ref 1 in
    for i = y+1 to y+p do
          if i < max_hauteur && i>= 0 then
          match tableau.(i).(x) with
          | Brique -> if !flagb then affiche_explosion x i p1 p2;
                if !flagb then remplace_brique_par_sol x i;
                if not bomb.explosion_brick then flagb := false;
          | Mur_Vert | Mur_Hor -> flagb := false;
          | _ -> if !flagb then affiche_explosion x i p1 p2;
    done

let champ_explosion_haut (bomb : bomb) (p1 : player) (p2 : player) = 
    let n = ref 0 in
    let y, x = bomb.position in
    let p = p1.power_bombe in 
    for i = y+1 to y+p do
        n := !n + 1;
        if i - 2 * !n  < max_hauteur && i - 2 * !n >= 0 then
            match tableau.(i - 2 * !n).(x) with
          | Brique -> if !flagh then affiche_explosion x (i - 2 * !n) p1 p2;
                if !flagh then remplace_brique_par_sol x (i - 2 * !n);
                if not bomb.explosion_brick then flagh := false;
          | Mur_Vert | Mur_Hor -> flagh := false;
          | _ -> if !flagh then affiche_explosion x (i - 2 * !n) p1 p2;
    done

let champ_explosion_gauche (bomb : bomb) (p1 : player) (p2 : player) = 
    let n = ref 0 in
    let y, x = bomb.position in
    let p = p1.power_bombe in 
    for i = x+1 to x+p do
        n := !n + 1;
        if i - 2 * !n  < max_longueur && i - 2 * !n >= 0 then
            match tableau.(y).(i - 2 * !n) with
          | Brique -> if !flagg then affiche_explosion (i - 2 * !n) y p1 p2;
                if !flagg then remplace_brique_par_sol (i - 2 * !n) y;
                if not bomb.explosion_brick then flagg := false;
          | Mur_Vert | Mur_Hor -> flagg := false; 
          | _ -> if !flagg then affiche_explosion (i - 2 * !n) y p1 p2;
    done

let champ_explosion_droite (bomb : bomb) (p1 : player) (p2 : player) = 
    let y, x = bomb.position in
    let p = p1.power_bombe in 
    for i = x+1 to x+p do
        if i < max_longueur && i>= 0 then
          match tableau.(y).(i) with
          | Brique -> if !flagd then affiche_explosion i y p1 p2;
                if !flagd then remplace_brique_par_sol i y;
                if not bomb.explosion_brick then flagd := false;
          | Mur_Vert | Mur_Hor -> flagd := false; 
          | _ -> if !flagd then affiche_explosion i y p1 p2;
    done

(* fonctions qui comparent les cases du plateau avec la position de la bombe
et sa puissance dans les 4 directions pour ensuite utiliser la fonction 
d'affichage sur les cases éligibles *)

let explosion_globale (bomb : bomb) (p1 : player) (p2 : player)=
    bomb.explosion_brick <- p1.explosion_brick;
    let y, x = bomb.position in
    let d = bomb.duree_explosion in 
    match d with
    |0 -> ()
    |_ -> champ_explosion_centre bomb p1 p2;
        champ_explosion_bas bomb p1 p2;
        champ_explosion_haut bomb p1 p2;
        champ_explosion_droite bomb p1 p2;
        champ_explosion_gauche bomb p1 p2;
        bomb.duree_explosion <- d-1


let affiche_compteur (j : player) (j2 : player) =
    draw_text "Vie : " 50  (25 + 11 * 46) 50 Color.green;
    draw_text (string_of_int j.vie) (50 + 3 * 46) (25 + 11 * 46) 50 Color.green;
    draw_text (string_of_int j.power_bombe) (50 + 7 * 46) (25 + 12 * 46) 50 Color.green;
    draw_text  "Puissance :" 50 (25 + 12 * 46) 50 Color.green;
    draw_text "Bombes :" 50 (25 + 13 * 46) 50 Color.green;
    draw_text (string_of_int j.nombre_bombe) (50 + 5 * 46) (25 + 13 * 46) 50 Color.green;
    draw_text "Vie : " (50 + 10 * 46)  (25 + 11 * 46) 50 Color.purple;
    draw_text (string_of_int j2.vie) (50 + 13 * 46) (25 + 11 * 46) 50 Color.purple;
    draw_text (string_of_int j2.power_bombe) (50 + 17 * 46) (25 + 12 * 46) 50 Color.purple;
    draw_text  "Puissance :" (50 + 10 * 46) (25 + 12 * 46) 50 Color.purple;
    draw_text "Bombes :" (50 + 10 * 46) (25 + 13 * 46) 50 Color.purple;
    draw_text (string_of_int j2.nombre_bombe) (50 + 15 * 46) (25 + 13 * 46) 50 Color.purple

(* fonction globale sur l'explosion et sur la durée d'affichage de l'explosion*)

let compteur_decr (bomb : bomb) (p1 : player) (p2 : player) =
    let t = bomb.compteur in
    match t with
    | 0 -> if bomb.duree_explosion = 0 then bomb.position <- (1500, 2000);
        explosion_globale bomb p1 p2
    | 1 -> p1.nombre_bombe <- p1.nombre_bombe + 1;
        bomb.duree_explosion <- 1;
        bomb.compteur <- 0;
        explosion_globale bomb p1 p2
    | _ -> bomb.compteur <- t - 1;
        flagb := true;
        flagh := true;
        flagg := true;
        flagd := true;
        explosion_globale bomb p1 p2

(* fonction qui agit sur le compteur de la bombe*)

(*création des bombes et des joueurs*)

let creer_bomberman x y s rb eb pb nb =
    {position = x,y; vie = 1; speed = s; repousse_bombe = rb; explosion_brick = eb;
    power_bombe = pb; nombre_bombe = nb}

let creer_bomb x y c e =
    {position = y, x; compteur = c; explosion_brick = e; duree_explosion = 0}

let bomberman = creer_bomberman 1 1 1 false false 1 1

let bomberman2 = creer_bomberman 9 13 1 false false 1 1

let b1 = creer_bomb 1500 2000 0 bomberman.explosion_brick
let b2 = creer_bomb 1500 2000 0 bomberman.explosion_brick
let b3 = creer_bomb 1500 2000 0 bomberman.explosion_brick
let b4 = creer_bomb 1500 2000 0 bomberman.explosion_brick
let b5 = creer_bomb 1500 2000 0 bomberman.explosion_brick

let b1_2 = creer_bomb 1500 2000 0 bomberman2.explosion_brick
let b2_2 = creer_bomb 1500 2000 0 bomberman2.explosion_brick
let b3_2 = creer_bomb 1500 2000 0 bomberman2.explosion_brick
let b4_2 = creer_bomb 1500 2000 0 bomberman2.explosion_brick
let b5_2 = creer_bomb 1500 2000 0 bomberman2.explosion_brick

(*fonction sur le depot de la bombe sur le sol*)
let depose (p1 : player) = 
    let y, x = p1.position in
    if is_key_pressed(L) then
        match p1.nombre_bombe with
        | 1 -> b1.position <- (y, x); b1.compteur <- 180 ; p1.nombre_bombe <- p1.nombre_bombe - 1
        | 2 -> b2.position <- (y, x); b2.compteur <- 180 ; p1.nombre_bombe <- p1.nombre_bombe - 1
        | 3 -> b3.position <- (y, x); b3.compteur <- 180 ; p1.nombre_bombe <- p1.nombre_bombe - 1
        | 4 -> b4.position <- (y, x); b4.compteur <- 180 ; p1.nombre_bombe <- p1.nombre_bombe - 1
        | 5 -> b5.position <- (y, x); b5.compteur <- 180 ; p1.nombre_bombe <- p1.nombre_bombe - 1
        | _ -> ()

let depose2 (p2 : player) = 
    let y, x = p2.position in
    if is_key_pressed(R) then
        match p2.nombre_bombe with
        | 1 -> b1_2.position <- (y, x); b1_2.compteur <- 180 ; p2.nombre_bombe <- p2.nombre_bombe - 1
        | 2 -> b2_2.position <- (y, x); b2_2.compteur <- 180 ; p2.nombre_bombe <- p2.nombre_bombe - 1
        | 3 -> b3_2.position <- (y, x); b3_2.compteur <- 180 ; p2.nombre_bombe <- p2.nombre_bombe - 1
        | 4 -> b4_2.position <- (y, x); b4_2.compteur <- 180 ; p2.nombre_bombe <- p2.nombre_bombe - 1
        | 5 -> b5_2.position <- (y, x); b5_2.compteur <- 180 ; p2.nombre_bombe <- p2.nombre_bombe - 1
        | _ -> ()

(*fonction sur les déplacements des joueurs , exemple key_bind*)
let deplacement x y (p : player) =
    match tableau.(y).(x) with
    |Sol -> p.position <- (y , x);
    |B_explosion -> p.position <- (y , x);
        p.explosion_brick <- true;
        remplace_autre_par_sol x y;
    |B_power -> p.position <- (y , x);
        p.power_bombe <- p.power_bombe + 1;
        remplace_autre_par_sol x y;
    |B_bombe_plus -> p.position <- (y , x);
        remplace_autre_par_sol x y;
        if p.nombre_bombe < nb_max_bombe then p.nombre_bombe <- p.nombre_bombe + 1;
    | _ -> ()

let mouvement_p1 (p1 : player) =
    let y, x = p1.position in
	if is_key_pressed(Left) then 
		deplacement (x - 1) y p1;
	if is_key_pressed(Right) then
		deplacement (x + 1) y p1;
	if is_key_pressed(Up) then
		deplacement x (y - 1) p1;
	if is_key_pressed(Down) then
		deplacement x (y + 1) p1;
    depose p1;
;;

let mouvement_p2 (p2 : player) =
    let y, x = p2.position in
	if is_key_pressed(A) then 
		deplacement (x - 1) y p2;
	if is_key_pressed(D) then
		deplacement (x + 1) y p2;
	if is_key_pressed(W) then
		deplacement x (y - 1) p2;
	if is_key_pressed(S) then
		deplacement x (y + 1) p2;
    depose2 p2;
;;

(* creation de la fenetre du jeu*)
let setup() =
        Raylib.init_window largeur_ecran hauteur_ecran "BOMBERMANSCII";
        Raylib.set_target_fps 60

(*affichage des differents ecrans du jeucomme l'ecran titre ou les ecrans de fins *)

let rec affiche_ecran_fin_1 () =
    if Raylib.window_should_close () then Raylib.close_window ()
    else
        let open Raylib in
        begin_drawing ();
        clear_background Color.black;
        draw_rectangle 0 0 largeur_ecran hauteur_ecran Color.black;
        draw_text "AND THE WINNER IS..." 40 40 50 Color.white;
        draw_text "PLAYER 1!" 200 200 60 Color.green;
        end_drawing();
        affiche_ecran_fin_1 ()
       
let rec affiche_ecran_fin_2 () =
    if Raylib.window_should_close () then Raylib.close_window ()
    else
        let open Raylib in
        begin_drawing ();
        clear_background Color.black;
        draw_rectangle 0 0 largeur_ecran hauteur_ecran Color.black;
        draw_text "AND THE WINNER IS..." 40 40 50 Color.white;
        draw_text "PLAYER 2!" 200 200 60 Color.purple;
        end_drawing();
        affiche_ecran_fin_2 ()
       
       
       
let rec affiche_ecran_fin_3 () =
        if Raylib.window_should_close () then Raylib.close_window ()
        else
            let open Raylib in
            begin_drawing ();
            clear_background Color.black;
            draw_rectangle 0 0 largeur_ecran hauteur_ecran Color.black;
            draw_text "That's a DRAW! " 40 40 50 Color.white;
            end_drawing();
            affiche_ecran_fin_3 ()

let affiche_map () = 
    draw_rectangle 0 0 largeur_ecran hauteur_ecran Color.black;
    for i = 0 to max_longueur-1 do
        for j = 0 to max_hauteur-1 do
            draw_text (tile_to_string (tableau.(j).(i))) (50 + i * 46) (25 + j * 46) 50 (associe_couleur tableau.(j).(i))
        done
    done

let est_mort (p1 : player) (p2 : player) =
    match p1.vie, p2.vie with
    | (0, 0) -> affiche_ecran_fin_3 ()
    | (1, 0) -> affiche_ecran_fin_1 ()
    | (0, 1) -> affiche_ecran_fin_2 ()
    | (_, _) -> ()

(*fonction qui appelle toutes les fonctions pour le deroulement du jeu*)
let rec boucle () =
    if Raylib.window_should_close () then Raylib.close_window ()
    else
        let open Raylib in
        begin_drawing ();
        clear_background Color.black;
        affiche_map ();
        affiche_bomberman bomberman;
        affiche_bomberman2 bomberman2;
        affiche_compteur bomberman bomberman2;
        affiche_bomb b1;
        affiche_bomb b2;
        affiche_bomb b3;
        affiche_bomb b4;
        affiche_bomb b5;
        affiche_bomb b1_2;
        affiche_bomb b2_2;
        affiche_bomb b3_2;
        affiche_bomb b4_2;
        affiche_bomb b5_2;
        compteur_decr b1 bomberman bomberman2;
        compteur_decr b2 bomberman bomberman2;
        compteur_decr b3 bomberman bomberman2;
        compteur_decr b4 bomberman bomberman2;
        compteur_decr b5 bomberman bomberman2;
        compteur_decr b1_2 bomberman2 bomberman;
        compteur_decr b2_2 bomberman2 bomberman;
        compteur_decr b3_2 bomberman2 bomberman;
        compteur_decr b4_2 bomberman2 bomberman;
        compteur_decr b5_2 bomberman2 bomberman;
        mouvement_p1 bomberman;
        mouvement_p2 bomberman2;
        est_mort bomberman bomberman2;
        end_drawing ();
        boucle ()

let rec affiche_ecran_titre () =
    if Raylib.window_should_close () then Raylib.close_window ()
    else if  is_key_pressed(Enter) then boucle ()
    else 
        let open Raylib in
        begin_drawing();
        clear_background Color.black;
        draw_rectangle 0 0 largeur_ecran hauteur_ecran Color.black;
        draw_text "BOMBERMANSCII" 100 50 75 Color.white;
        draw_text "Please PRESS ENTER to " 50 200 50 Color.white;
        draw_text "Start the GAME! " 50 275 50 Color.white;
        end_drawing();
        affiche_ecran_titre ()

let () =
    map_tile ();
    setup () |> affiche_ecran_titre
