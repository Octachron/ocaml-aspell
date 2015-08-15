open Ctypes
open Foreign    


module Result = struct
  type t
  let t : t structure typ = structure "AspellCanHaveError"
  let id = foreign "aspell_error_number" ( ptr t @-> returning int ) 
  let msg = foreign "aspell_error_message" ( ptr t @-> returning string )
  
  
end
  
module Config = struct
  type t
  let t : t structure typ = structure "AspellConfig"         
  let pt = ptr t
  
  let create = foreign "new_aspell_config" ( void @-> returning pt )
  let replace = foreign "aspell_config_replace" ( ptr t @-> string @-> string @-> returning void)
  let clone = foreign "aspell_config_clone" ( ptr t @-> returning pt )
  let delete = foreign "delete_aspell_config" ( ptr t @-> returning void )
  let with_ f =
           let d = create () in
           let x = f d in
           delete d; x
    
end

module Speller = struct
  type t
  let t : t structure typ = structure "AspellSpeller"
  type list
  let list: list structure typ = structure "AspellWordList"
  type sequence
  let sequence: sequence structure typ = structure "AspellStringEnumeration"
    
  
  let create = foreign "new_aspell_speller" ( ptr Config.t @-> returning (ptr Result.t) )
  let delete = foreign "delete_aspell_speller" ( ptr t @-> returning void )
  
  let unwrap = foreign "to_aspell_speller" ( ptr Result.t @-> returning (ptr t) )
  let check = foreign "aspell_speller_check" (ptr t @-> string @-> int @-> returning int)

  let suggest =  foreign "aspell_speller_check" (ptr t @-> string @-> int @-> returning (ptr list) )
  let elements = foreign "aspell_word_list_elements" ( ptr list @-> returning (ptr sequence) )
  let next = foreign "aspell_string_enumeration_next" ( ptr sequence @-> returning string ) (* nullable *)
  let store_replace = foreign "aspell_speller_store_replacement" (ptr t @-> string @-> string @-> int @-> returning void )

  let add_to_session = foreign "aspell_speller_add_to_session" (ptr t @-> string @-> int @-> returning void)
  let add_to_personal = foreign "aspell_speller_add_to_personal" (ptr t @-> string @-> int @-> returning void)
  
end

let test =
  let conf = Config.create () in
  let spell_opt = Speller.create conf in
  if Result.id spell_opt <> 0 then
    ( Printf.printf "%s\n" (Result.msg spell_opt); assert false )
  else
    let spell = Speller.unwrap spell_opt in 
    Printf.printf "%d \n" @@  Speller.check spell "helli" 5

(*
 AspellCanHaveError * possible_err = new_aspell_speller(spell_config);
     AspellSpeller * spell_checker = 0;
     if (aspell_error_number(possible_err) != 0)
       puts(aspell_error_message(possible_err));
     else
       spell_checker = to_aspell_speller(possible_err);

     AspellConfig * spell_config2 = aspell_config_clone(spell_config);
     aspell_config_replace(spell_config2, "lang","nl");
     possible_err = new_aspell_speller(spell_config2);
     delete_aspell_config(spell_config2);

     int correct = aspell_speller_check(spell_checker, word, size);




*)
