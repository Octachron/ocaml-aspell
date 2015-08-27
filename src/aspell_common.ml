open Ctypes
open Foreign


let default x = function None -> x | Some x -> x
let show_list l =
  let rec show =
    function
    | [] -> " ]"
    | [a] -> a ^ " ]" 
    | a::r -> a ^" ; " ^ show r
  in 
  "[ " ^ show l 

let string_flat =
  view ~read:(default "") ~write:(function "" -> None | s -> Some s)
    string_opt
      
let names core = core, core ^ "_", "_"^ core

(** Configuration key *)
module Key = struct
  
  (** The type of the key *)
  type kind = String | Int | Bool | List
  let show_kind = function
    | String -> "string"
    | Int -> "int"
    | Bool -> "bool"
    | List -> "list"
  
  let kind =
    view
      ~read:( function 0 -> String | 1 -> Int | 2 -> Bool | 3 -> List | _ -> assert false) 
      ~write: ( function String -> 0 | Int -> 1 | Bool -> 2 | List -> 3 )
      int

  type value =
    | String of string
    | Int of int
    | Bool of bool
    | List of string list


  
  let rec show_value = function
    | String s -> s
    | Int n -> string_of_int n
    | Bool b -> if b then "true" else "false"
    | List l -> show_list l
    
  type c
  let c : c structure typ = structure "AspellKeyInfo"
  let ptr_c = ptr c

  (** The name of the key *)
  let name = field c "name" string_flat
  (** Key type *)
  let type_ = field c "type_" kind
  (** Default value *) 
  let def = field c "def" string_flat
  (** Brief description *)
  let desc = field c "desc" string_flat
  (** Flags? *)
  let flags = field c "flags" int
  (** Other data?? *)
  let other_data = field c "other_data" int
  let () = seal c

  type info = {
    name: string
  ; type_: kind
  ; def: string
  ; desc: string
  ; flags: int
  ; other_data : int
  }

  let t_gen ~read ~write prim = view
      ~read:(fun x ->
          let x = read x in
          let g f = getf x f in
          { name = g name
          ; type_ = g type_
          ; def = g def
          ; desc = g desc
          ; flags = g flags
          ; other_data = g other_data
          }
        )
      ~write: (fun x ->
          let y = make c in
          let s f= setf y f in
          let () =
            s name x.name
          ; s type_ x.type_
          ; s def x.def
          ; s desc x.desc
          ; s flags x.flags
          ; s other_data x.other_data in
          write y
        )
      prim

  let id x = x

  let info = t_gen ~read:id ~write:id c
  
  let ptr_info = t_gen
      ~read:(fun x -> !@ x )
      ~write:( fun x -> allocate c x)
      (ptr c)

  let show k =
    Printf.sprintf "{ \
 name = %s\
  ; type_ = %s\
  ; def = %s\
  ; desc = %s\
  ; flags = %d\
  ; other_data = %d }"
 k.name
 (show_kind k.type_)
 k.def
 k.desc
 k.flags
 k.other_data
  
end
