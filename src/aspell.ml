open Ctypes
open Foreign    

let names core = core, core ^ "_", "_"^ core

module Type_id = struct
  type t
  let t : t union typ = union "AspellTypeId"

  let num = field t "num" uint
  let str = field t "str" string
  let () = seal t
end

module Mutable_container = struct
  type t
  let prim_t : t structure typ = structure "AspellMutableContainer"
  let t = ptr prim_t
  let add = foreign "aspell_mutable_container_add" @@ t @-> string @-> returning bool
  let remove = foreign "aspell_mutable_container_remove" @@ t @-> string @-> returning bool
  let clear =  foreign "aspell_mutable_container_clear" @@ t @-> returning void 
  let convert =  foreign  "aspell_mutable_container_to_mutable_container" @@ t @-> returning t
end

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
  let to_list seq =
    let rec build l =
      match next seq with
      | None -> List.rev l
      | Some a -> build (a::l) in
    build []
    
end

module StringPair = struct
  type s
  let t : s structure typ = structure "AspellStringPair"
  let first = field t "first" string
  let second = field t "second" string
end

module StringPairEnum = Make_enum (struct
    let s_name =  "AspellStringPairEnumeration"
    let core_name = "aspell_string_pair_enumeration"
    type r = StringPair.s structure Ctypes_static.ptr
    let r = ptr_opt StringPair.t
    end
  )

type 'a colored_int = private int

(** Configuration key *)
module Key_info = struct
  
  (** The type of the key *)
  type kind = String | Int | Bool | List
  let kind =
    view
      ~read:( function 0 -> String | 1 -> Int | 2 -> Bool | 3 -> List | _ -> assert false) 
      ~write: ( function String -> 0 | Int -> 1 | Bool -> 2 | List -> 3 )
      int
  
  type t
  let t : t structure typ = structure "AspellKeyInfo"
  let ptr_t = ptr t

  (** The name of the key *)
  let name = field t "name" string
  (** Key type *)
  let type_ = field t "type_" kind
  (** Default value *) 
  let def = field t "def" string
  (** Brief description *)
  let desc = field t "desc" string
  (** Flags? *)
  let flags = field t "flags" int
  (** Other data?? *)
  let other_data = field t "other_data" int
  let () = seal t
end

module Key_list = struct
  type t
  let prim_t : t structure typ = structure "AspellKeyInfoEnumeration"
  let t = ptr prim_t
  let is_at_end = foreign "aspell_key_info_enumeration_at_end" @@ t @-> returning bool
  let next  =foreign "aspell_key_info_enumeration_next" @@ t @-> returning t 
  let clone = foreign "aspell_key_info_enumeration_clone" @@ t @-> returning t
  let assign = foreign "aspell_key_info_enumeration_assign" @@ t @-> t @-> returning void

end

module Error = struct
  type info
  let info : info structure typ = structure "AspellErrorInfo"
  let isa = field info "isa" @@ ptr info
  let mesg = field info "mesg" string
  let num_parms = field info "num_parms" uint
  let parms = field info "parms"@@ ptr string
  let () = seal info

  
  type t
  let t : t structure typ = structure "AspellError"
  let mesg = field t "mesg" string
  let err = field t "mesg" (ptr info)
  let () = seal t

let is_a = foreign "aspell_error_is_a" @@ ptr t @-> ptr info @-> returning bool

let bind prefix typ =
  let f name rtyp = foreign (prefix^"error"^name) @@ typ @-> returning rtyp in 
  f "_number" uint, f "_message" string, f "" (ptr t)

end

module Config = struct
  type t
  let prim_t : t structure typ = structure "AspellConfig"         
  let t = ptr prim_t

  let pre = "aspell_config_"
  let create = foreign "new_aspell_config" ( void @-> returning t )
  let replace = foreign "aspell_config_replace" ( t @-> string @-> string @-> returning void)
  let clone = foreign "aspell_config_clone" ( t @-> returning t )
  let delete = foreign "delete_aspell_config" ( t @-> returning void )
  let with_ f =
           let d = create () in
           let x = f d in
           delete d; x
  let assign = foreign "aspell_config_assign" @@ t @-> t @-> returning void

  let error_number, error_msg, error = Error.bind pre t
      
  (**
     Sets extra keys which this config class should
     accept. begin and end are expected to point to
     the beginning and ending of an array of Aspell
     Key Info.
  *)
  let set_extra = foreign "aspell_config_set_extra" @@ t @-> ptr Key_info.t @-> ptr Key_info.t @-> returning void

(** Returns the KeyInfo object for the
    corresponding key or returns NULL and sets
    error_num to PERROR_UNKNOWN_KEY if the key is
    not valid. The pointer returned is valid for
    the lifetime of the object. *)
  let key_info = foreign "aspell_config_keyinfo" @@ t @-> string @-> returning (ptr Key_info.t)
                                                      
  (** Returns a newly allocated enumeration of all
      the possible objects this config class uses. *)
  let possible_elements = foreign "aspell_config_possible_elements" @@ t @-> bool @-> returning Key_list.t

  (** Returns the default value for given key which
    may involve substituting variables, thus it is
    not the same as keyinfo(key)->def returns NULL
    and sets error_num to PERROR_UNKNOWN_KEY if
    the key is not valid. Uses the temporary
    string. *)
  let get_default = foreign "aspell_config_get_default" @@ t @-> string @-> returning string_opt


(**
 Returns a newly allocated enumeration of all
 the key/value pairs. This DOES not include ones
 which are set to their default values. */
*)
let elements = foreign "aspell_config_elements" @@ ptr t @-> returning ( ptr StringPairEnum.t)

(** Inserts an item, if the item already exists it
    will be replaced. Returns TRUE if it succeeded
    or FALSE on error. If the key is not valid it
    sets error_num to PERROR_UNKNOWN_KEY, if the
    value is not valid it will set error_num to
    PERROR_BAD_VALUE, if the value can not be
    changed it sets error_num to
    PERROR_CANT_CHANGE_VALUE, and if the value is
    a list and you are trying to set its directory,
    it sets error_num to PERROR_LIST_SET
 
    [replace config key value : bool ]
*)
let replace =foreign "aspell_config_replace" @@ t @-> string @-> string @-> returning bool

(** Remove a key and returns TRUE if it exists
    otherwise return FALSE. This effectively sets
    the key to its default value. Calling replace
    with a value of "<default>" will also call
    remove. If the key does not exist then it sets
    error_num to 0 or PERROR_NOT, if the key is
    not valid then it sets error_num to
    PERROR_UNKNOWN_KEY, if the value can not be
    changed then it sets error_num to
    PERROR_CANT_CHANGE_VALUE 
*)
let remove = foreign "aspell_config_remove" @@ t @-> string @-> returning bool

let mem = foreign "aspell_config_have" @@ t @-> string @-> returning bool
                                          
(** Returns NULL on error. *)
let find = foreign "aspell_config_retrieve" @@ t @-> string @-> returning string_opt
let find_list = foreign "aspell_config_retrieve_list" @@ t @-> string @-> Mutable_container.t @-> returning bool
(** In "ths" Aspell configuration, search for a
  character string matching "key" string.
  If "key" is found then return 1 else return 0.
    If error encountered, then return -1. *)
let find_bool = foreign "aspell_config_retrieve_bool" @@ t @-> string @-> returning int


(** In "ths" Aspell configuration, search for an
  integer value matching "key" string.
    Return -1 on error. *)
let find_int = foreign "aspell_config_retrieve_int" @@ t @-> string @-> returning int

end


module Result = struct
  type c
  let prim_c : c structure typ = structure "AspellCanHaveError"
  let c = ptr prim_c
  type 'a t = [ `Ok of 'a | `Error of string ]       
  let id = foreign "aspell_error_number" @@ c @-> returning int 
  let msg = foreign "aspell_error_message" @@ c @-> returning string
  let err = foreign "aspell_error" @@ c @-> returning @@  ptr Error.t
  let convert proj with_error =
    if id with_error = 0 then
      `Ok (proj with_error)
    else
      `Error (msg with_error)

  let delete = foreign "delete_aspell_can_have_error" @@ c @-> returning void
                                                           
end

    
module String_enum = Make_enum (struct
    let s_name = "AspellStringEnumeration"
    let core_name = "aspell_string_enumeration"
    type r = string
    let r = string_opt
  end)
    
module String_list = struct
  type t
  let prim_t : t structure typ = structure "AspellStringList"
  let t  = ptr prim_t
  let corename, pre,post = names "aspell_string_list"
  let fp name typ  = foreign (pre^name) typ
  let fpt name typ= foreign (name ^ post ) typ

  let create = fpt "new" @@ void @-> returning t
  let is_empty = fp "empty" @@ t @-> returning bool
  let size = fp "size" @@ t @-> returning int

  let elements = fp "elements" @@ t @-> returning String_enum.t
  let to_list c = String_enum.to_list @@ elements c

  
  let add = fp "add" @@ t @-> string @-> returning bool
  let remove = fp "remove" @@ t @-> string @-> returning bool

  let from_list l =
    let c = create () in
    List.iter (fun x -> ignore @@ add c x) l ;
    c
  
  let clear = fp "clear" @@ t @-> returning void

  let to_mutable_container = fp "to_mutable_container" @@ t @-> returning Mutable_container.t

  let delete = fpt "delete" @@ t @-> returning void
  let clone = fp "clone" @@ t @-> returning t
  let assign = fp "assign" @@ t @-> t @-> returning void
end

module Word_list = struct
  type t
  let prim_t: t structure typ = structure "AspellWordList"
  let t = ptr prim_t

  
  let is_empty = foreign "aspell_word_list_empty" ( t @-> returning bool )
  let size = foreign "aspell_word_list_size" ( t @-> returning int )

  let c_elements = foreign "aspell_word_list_elements" ( t @-> returning String_enum.t )

      
  let to_list wlist =
    String_enum.to_list @@ c_elements wlist 

end


  let (@@@) f x = f x (String.length x) 

module Speller = struct
  type t
  let prim_t : t structure typ = structure "AspellSpeller"
  let t = ptr prim_t
  let pre = "aspell_speller_"
  let fp name typ = foreign (pre^name) typ 
  
  
  let c_create = foreign "new_aspell_speller" ( Config.t @-> returning Result.c)

(** returns  0 if it is not in the dictionary,
    1 if it is, or -1 on error. *)
  let c_check = fp "check" @@ t @-> string @-> int @-> returning bool
  let unwrap = foreign "to_aspell_speller" ( Result.c @-> returning t )
  let c_suggest =  fp "suggest" @@ t @-> string @-> int @-> returning Word_list.t

  let c_store_replace = fp "store_replacement" @@ t @-> string @-> int @-> string @-> int @-> returning void

  let c_add_to_session = fp "add_to_session"@@  t @-> string @-> int @-> returning void
  let c_add_to_personal = fp "add_to_personal" @@ t @-> string @-> int @-> returning void
  
  let create config = Result.convert unwrap @@ c_create config 
  let delete = foreign "delete_aspell_speller" ( t @-> returning void )
  
  let check speller word = c_check speller @@@ word    
  let suggest speller word = Word_list.to_list @@ c_suggest speller @@@ word
  
  let store_replace speller mispelled replacement =
    c_store_replace speller @@@ mispelled @@@ replacement
  
  let add_to_session speller word= c_add_to_session speller @@@ word 
  let add_to_personal speller word= c_add_to_personal speller @@@ word 

  let error_number, error_msg, error = Error.bind pre t

  let config = fp "config" @@ t @-> returning Config.t                                                      
                                                              
  let c_personal_word_list = fp "personal_word_list" @@ t @-> returning Word_list.t
  let personal_word_list speller = Word_list.to_list @@ c_personal_word_list speller 

  let c_session_word_list = fp "session_word_list" @@ t @-> returning Word_list.t
  let session_word_list speller = Word_list.to_list @@ c_session_word_list speller 

  let c_main_word_list = fp "main_word_list" @@ t @-> returning Word_list.t
  let main_word_list speller = Word_list.to_list @@ c_main_word_list speller 

  let save_all_word_lists = fp "save_all_word_lists" @@ t @-> returning bool
  let clear_session = fp "clear_session" @@ t @-> returning bool
  
end

module Filter = struct
  type t
  let prim_t : t structure typ = structure "AspellFilter"
  let t = ptr prim_t
  let pre = "aspell_filter_"
  
  let fm name typ= foreign name ( t @-> typ )
  let fmp name typ = fm (pre ^ name) typ
  let delete = fm "delete_aspell_filter" @@ returning void
  let error_number, error_msg, error = Error.bind pre t
      
  let to_ = foreign  "to_aspell_filter" @@ Result.c @-> returning t
end



module Document_checker = struct
  type token
  let token: token structure typ = structure "AspellToken"
  let offset = field token "offset" uint
  let len = field token "len" uint
  let () = seal token

  type t
  let prim_t : t structure typ = structure "AspeelDocumentChecker"
  let t = ptr prim_t
  let core_name = "aspell_document_checker"
  let pre = core_name ^ "_"
  let post = "_" ^ core_name
  let fp name typ = foreign (pre^name) typ
  let fpt name typ = foreign (name^post) typ
      
  let delete = fpt "delete" @@ t @-> returning void
  let error_number, error_msg, error = Error.bind pre t                               
  


  (** Creates a new document checker.
    The speller class is expected to last until
    this class is destroyed.
    If config is given it will be used to override
    any relevent options set by this speller class.
    The config class is not once this function is done.
    If filter is given then it will take ownership of
    the filter class and use it to do the filtering.
      You are expected to free the checker when done. *)
  let c_create = fpt "new" @@ Speller.t @-> returning Result.c

  let unwrap = fpt "to" @@ Result.c @-> returning t
  let create speller = Result.convert unwrap @@ c_create speller                                

  
(** Reset the internal state of the filter.
  Should be called whenever a new document is
    being filtered. *)
let reset = fp "reset" @@ t @-> returning void

(** Process a string.
    The string passed in should only be split on
    white space characters.  Furthermore, between
    calls to reset, each string should be passed
    in exactly once and in the order they appeared
    in the document.  Passing in strings out of
    order, skipping strings or passing them in
    more than once may lead to undefined results. *)
let c_process = fp "process" @@ t @-> string @-> int @-> returning void
let process dc s = c_process dc @@@ s 

(** Returns the next misspelled word in the
    processed string.  If there are no more
    misspelled words, then token.word will be
    NULL and token.size will be 0 *)
let c_next_misspelling = fp "next_misspelling" @@ t @-> returning token
                                                    
(** Returns the underlying filter class. *)
let filter =  fp "filter" @@ t @-> returning Filter.t

end



module Info = struct
  module Module = struct 
    type s_t
    let t : s_t structure typ = structure "AspellModuleInfo"
    let name = field t "name" string
    let order_num = field t "order_num" double
    let lib_dir = field t "lib_dir" string
    let dict_dirs = field t "dict_dirs" String_list.t
    let dict_exts = field t "dict_exts" String_list.t     
    let () = seal t

  module Enum = Make_enum( struct
      let s_name = "AspellModuleInfoEnumeration"
      let core_name = "aspell_module_info_enumeration"
      type r = s_t structure Ctypes_static.ptr
      let r = ptr_opt t
    end)
  
  module List = struct
    type t
    let prim_t: t structure typ = structure "AspellModuleInfoList"
    let t = ptr prim_t
    let core_name, pre, post = names "aspell_module_info_list" 
    let fp name typ = foreign (pre^name) typ
    let fpt name typ = foreign (name^post) typ

    let get = fpt "get" @@ Config.t @-> returning t
    let is_empty = fp "empty" @@ t @-> returning bool
    let size = fp "size" @@ t @-> returning uint
    let c_elements = fp "elements" @@ t @-> returning Enum.t
  end
    
  end
  module Dict = struct
  type s_t
  let t : s_t structure typ = structure "AspellDictInfo"
  (** The Name to identify this dictionary by. *)
  let name = field t "name" string
  (** The language code to identify this dictionary.
      A two letter UPPER-CASE ISO 639 language code
      and an optional two letter ISO 3166 country
      code after a dash or underscore. *)
  let code = field t "code" string
      
  (** Any extra information to distinguish this
      variety of dictionary from other dictionaries
      which may have the same language and size. *)
  let jargon = field t "jargon" string

  let size = field t "size" int

  (** A two char digit code describing the size of
    the dictionary: 10=tiny, 20=really small,
      30=small, 40=med-small, 50=med, 60=med-large,
   70=large, 80=huge, 90=insane.  Please check
   the README in aspell-lang-200?????.tar.bz2 or
   see SCOWL (http://wordlist.sourceforge.net)
      for an example of how these sizes are used. *)
      
  let size_str = field t "size_str" string

  let module_ = field t "module" @@ ptr Module.t



  module Enum = Make_enum( struct
      let s_name = "AspellDictInfoEnumeration"
      let core_name = "aspell_dict_info_enumeration"
      type r = s_t structure Ctypes_static.ptr
      let r = ptr_opt t
    end)

  module List = struct
    type t
    let prim_t: t structure typ = structure "AspellDictInfoList"
    let t = ptr prim_t
    let core_name, pre, post = names "aspell_dict_info_list" 
    let fp name typ = foreign (pre^name) typ
    let fpt name typ = foreign (name^post) typ

    let get = fpt "get" @@ Config.t @-> returning t
    let is_empty = fp "empty" @@ t @-> returning bool
    let size = fp "size" @@ t @-> returning uint
    let c_elements = fp "elements" @@ t @-> returning Enum.t
  end
  
  end
  
end


 (** Reset the global cache(s) so that cache queries will
     create a new object. If existing objects are still in
     use they are not deleted. If which is NULL then all
     caches will be reset. Current caches are "encode",
     "decode", "dictionary", "language", and "keyboard". *)
let c_reset_cache = foreign "aspell_reset_cache" @@ string @-> returning bool



