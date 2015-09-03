open Aspell
open Aspell_errors

let default = Aspell_common.default

let pp = Printf.printf

let fpp = Printf.fprintf

let pp_repeat n pp ppf x =
  for i = 1 to n do
    pp ppf x
  done

let pp_line ppf =
 pp_repeat 80 (fun ppf () -> Printf.fprintf ppf "-" ) ppf

let rec pp_list ?(sep=fun ppf () -> fpp ppf "; ") pp ppf = function
  | [] -> ()
  | [a] -> pp ppf a
  | a::q -> pp ppf a; sep ppf (); pp_list pp ppf q

let pp_string ppf s = fpp ppf "%s" s 

(*
let () =
  pp "Aspell version: %s \n" @@ version ()
*)
let ($) b msg =
  if b then () else Printf.printf msg

let (>>?) x f = match x with
  | Some x -> f x
  | None -> ()

module Test0 = struct

  let () =
    List.iter (fun e -> pp "%s\n" @@ Error.show e ) Aspell_errors.other
    
  
  let conf = Config.create ()
  let pk k = pp "%s \n" @@ Key.show k
 
  let () = 
    let k= Config.key_info conf "lang" in
    k >>? pk
      
  let () =
    let possible_keys = Config.possible_elements conf false in
    List.iter pk possible_keys

  let () =
    let open Aspell.Key in
    let s = match Config.find conf "reverse" with
      | Some (Bool b) -> if b then "true" else "false"
      | None | Some _ -> "none" in 
    pp "\n-----------\n reverse: %s \n----------\n" s

  let () =
    let open Aspell.Key.Typed in
    let open Config.Typed in
    Config.key_info conf "reverse" >>? fun info -> 
    let reverse = convert Bool info in
    find conf reverse >>? fun b -> pp "\n%a\n reverse:%b \n%a\n" pp_line () b pp_line ()
  
  
    let () = 
      let d = Config.get_default conf "lang" in
      Printf.printf " lang default: %s\n" @@ default "" d

  let () =
    let open Aspell.Key in
    let l = match Config.find conf "filter" with
      | Some (List l) -> l 
      | None | Some _ -> [] in 
    pp "\n%a\n filter: %a\n%a\n"
      pp_line ()
      (pp_list pp_string ) l
      pp_line ()

  let () =
    pp " %a\n replace reverse: %b\n %a \n"
      pp_line ()
      (Config.replace conf "reverse" "exotic"  )
      pp_line ()

  let () = 
     pp " %a\n replace dict-alias: %b\n"
      pp_line ()
      (Config.replace conf "dict-alias" "exotic:really:truly"  )
     ;
     let l = match Config.find conf "dict-alias" with
       | Some (Key.List l) -> l 
       | None | Some _ -> [] in 
     pp "dict-alias: %a\n%a\n"
       (pp_list pp_string ) l
       pp_line ()

  
end



let test =
  let conf = Config.create () in
  let () = Config.replace conf "lang" "en_US"  $ "Failed" in
  let spell_opt = Speller.create conf in
  match spell_opt with
  | `Error msg -> let () = Printf.printf "%s\n" msg in assert false
  | ` Ok spell ->
    let m = Speller.suggest spell "hello" in
    Printf.printf "len:%d\n" @@ List.length m;
    List.iter (Printf.printf "%s \n")  @@  m;
    let open Speller in
    List.iter (add_to Session spell) ["anticommutator";"sesquilinear"];
    pp "[%a]\n" (pp_list pp_string) @@ Speller.word_list Session spell;
    pp "[%a]\n" (pp_list pp_string) @@ Speller.word_list Personal spell
