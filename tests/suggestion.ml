open Aspell
open Errors

let test =
  let conf = Config.create () in
  let spell_opt = Speller.create conf in
  match spell_opt with
  | `Error msg -> let () = Printf.printf "%s\n" msg in assert false
  | ` Ok spell ->
    let m = Speller.suggest spell "hello" in
    Printf.printf "len:%d\n" @@ List.length m;
    List.iter (Printf.printf "%s \n")  @@  m
