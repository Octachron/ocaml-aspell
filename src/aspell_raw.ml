open Ctypes
open Foreign
open Aspell_common

module Type_id = struct
  type t
  let t : t union typ = union "AspellTypeId"

  let num = field t "num" uint
  let str = field t "str" string
  let () = seal t
end

let opt t = view
    ~read:(function None -> None | Some x -> Some (!@ x) )
    ~write:(function None -> None | Some x -> Some( allocate t x ) )
    (ptr_opt t)


module Mutable_container = struct
  type t
  let prim_t : t structure typ = structure "AspellMutableContainer"
  let t = ptr prim_t
  let add = foreign "aspell_mutable_container_add" @@ t @-> string @-> returning bool
  let remove = foreign "aspell_mutable_container_remove" @@ t @-> string @-> returning bool
  let clear =  foreign "aspell_mutable_container_clear" @@ t @-> returning void 
  let convert =  foreign  "aspell_mutable_container_to_mutable_container" @@ t @-> returning t
end

let to_list next seq =
  let rec build l =
    match next seq with
    | None -> List.rev l
    | Some a -> build (a::l) in
  build []
    
module Make_enum( Core:
                  sig
                    val s_name: string
                    val core_name : string
                    type r
                    val r : r option Ctypes_static.typ 
                  end ) =
struct
  type t
  let prim_t: t structure typ = structure Core.s_name
  let t = ptr prim_t
  let core_name, pre, post = names Core.core_name
  let fp name typ = foreign (pre^name) typ
  let fpt name typ = foreign (name^post) typ

  let delete = fpt "delete" @@ t @-> returning void
  let clone = fp "clone" @@ t @-> returning t
  let assign = fp "assign" @@ t @-> t @-> returning t
  let at_end = fp "at_end" @@ t @-> returning bool
  let next = fp "next" @@ t @-> returning Core.r (* nullable *)
  let to_list = to_list next
  let listify f x = to_list @@ f x
end


module StringPair = struct
  type s
  let prim : s structure typ = structure "AspellStringPair"
  let first = field prim "first" string
  let second = field prim "second" string
  let t =
    view
      ~read:(fun r -> getf r first, getf r second )
      ~write: (fun (f,s) ->
          let r = make prim in
          setf r first f;
          setf r second s;
          r )
      prim
end


module StringPairEnum = Make_enum (struct
    let s_name =  "AspellStringPairEnumeration"
    let core_name = "aspell_string_pair_enumeration"
    type r = string * string
    let r = opt StringPair.t
    end
  )


module String_enum = Make_enum (struct
    let s_name = "AspellStringEnumeration"
    let core_name = "aspell_string_enumeration"
    type r = string
    let r = string_opt
  end)
    
module String_list = struct
  type t
  let prim_t : t structure typ = structure "AspellStringList"
  let c  = ptr prim_t
  let corename, pre,post = names "aspell_string_list"
  let fp name typ  = foreign (pre^name) typ
  let fpt name typ= foreign (name ^ post ) typ

  let create = fpt "new" @@ void @-> returning c
  let is_empty = fp "empty" @@ c @-> returning bool
  let size = fp "size" @@ c @-> returning int

  let elements = fp "elements" @@ c @-> returning String_enum.t
  let to_list c = String_enum.to_list @@ elements c
  let listify f x = to_list @@ f x 
  
  let add = fp "add" @@ c @-> string @-> returning bool
  let remove = fp "remove" @@ c @-> string @-> returning bool

  let from_list l =
    let c = create () in
    List.iter (fun x -> ignore @@ add c x) l ;
    c
  
  let clear = fp "clear" @@ c @-> returning void

  let to_mutable_container = fp "to_mutable_container" @@ c @-> returning Mutable_container.t

  let delete = fpt "delete" @@ c @-> returning void
  let clone = fp "clone" @@ c @-> returning c
  let assign = fp "assign" @@ c @-> c @-> returning void

  
  let t = view
      ~read:to_list
      ~write:from_list
      c

end

module Word_list = struct
  type t
  let prim_t: t structure typ = structure "AspellWordList"
  let c = ptr prim_t
  let t = c
  
  let is_empty = foreign "aspell_word_list_empty" ( c @-> returning bool )
  let size = foreign "aspell_word_list_size" ( c @-> returning int )

  let c_elements = foreign "aspell_word_list_elements" ( c @-> returning String_enum.t )

      
  let to_list wlist =
    String_enum.to_list @@ c_elements wlist
  let listify f x = to_list @@ f x

end

module Key_enum = Make_enum (struct
    let s_name = "AspellKeyInfoEnumeration"
    let core_name = "aspell_key_info_enumeration"
    type r = Key.info
    let r = opt Key.info
  end)
