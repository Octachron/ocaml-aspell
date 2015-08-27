open Aspell
open Aspell_errors

let default = Aspell_common.default
    
let pp = Printf.printf
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
      let d = Config.get_default conf "lang" in
      Printf.printf " lang default: %s\n" @@ default "" d

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
    List.iter (Printf.printf "%s \n")  @@  m
