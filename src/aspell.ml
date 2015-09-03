open Ctypes
open Foreign
open Aspell_common
open Aspell_raw

(* let version = foreign "aspell_version_string" @@ void @-> returning string; *)

type 'a colored_int = private int

module Key = Aspell_common.Key

module Error = struct

  
  type info_c
  let info : info_c structure typ = structure "AspellErrorInfo"
  let isa = field info "isa" @@ ptr_opt info
  let mesg = field info "mesg" string_flat
  let num_parms = field info "num_parms" int
  let parms = field info "parms"@@ ptr string_flat
  let () = seal info

  let mesg_i = mesg
  
  type info = {
    mesg: string
  ; num_parms: int
  ; parms : string array          
  }


  let show {mesg;num_parms;parms} =
    Array.fold_left (Printf.sprintf "%s,\n%s") 
      ( Printf.sprintf "Error info: %s %d\n" mesg num_parms )
      parms 

  
  let info =
    let rec read x =
      let i =
        {
          mesg = getf x mesg
        ; num_parms = getf x num_parms
        ; parms = Array.make 3 "" (*Array.of_list @@ CArray.( to_list @@ from_ptr (getf x parms) 3 )*)
        } in
      match getf x isa with
      | None -> [i]
      | Some isa -> i :: read (!@isa) in
    let rec write =
      function
      | [] -> assert false
      | [a] -> write_elt a None
      | a::b -> write_elt a @@ Some (allocate info @@ write b)
  and write_elt a q =                   
        let r = make info in
        let () =
          setf r mesg a.mesg
        ; setf r num_parms @@ a.num_parms
        ; setf r parms @@ ( CArray.start ( CArray.of_list string @@ Array.to_list a.parms ) )
        ; setf r isa @@  q in
       r in
    view ~read ~write info
  
  
  type c
  let c : c structure typ = structure "AspellError"
  let mesg = field c "mesg" string_flat
  let err = field c "mesg" info
  let () = seal c

  type t = {
    mesg : string
  ; err : info list  
  } 

  let t_gen ~read ~write t =
    let read x = let x = read x in {
      mesg = getf x mesg;
      err = getf x err
    } 
    and write x =
      let r = make c in
      let () =
        setf r mesg x.mesg
      ; setf r err x.err in
      write r
    in view ~read ~write t

  let id x = x
  let t =t_gen ~read:id ~write:id c
  let ptr_t = t_gen ~read:(fun x -> !@ x) ~write:(fun x -> allocate c x) (ptr c)
  
let is_a = foreign "aspell_error_is_a" @@ ptr_t @-> ptr info @-> returning bool

let bind prefix typ =
  let f name rtyp = foreign (prefix^"error"^name) @@ typ @-> returning rtyp in 
  f "_number" uint, f "_message" string, f "" ptr_t

end

module Config = struct
  type prim
  type t = prim Ctypes.structure Ctypes_static.ptr
      
  let prim_t : prim structure typ = structure "AspellConfig"         
  let t : t typ = ptr prim_t

  let pre = "aspell_config_"
  let create : unit -> t = foreign "new_aspell_config" ( void @-> returning t )
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
  let set_extra conf extras =
    let c = foreign "aspell_config_set_extra" @@ t @-> ptr Key.info @-> ptr Key.info @-> returning void in
    let a = CArray.of_list Key.info  extras in
    let s = CArray.start a in
    let e = s +@ CArray.length a in
    c conf s e

(** Returns the KeyInfo object for the
    corresponding key or returns NULL and sets
    error_num to PERROR_UNKNOWN_KEY if the key is
    not valid. The pointer returned is valid for
    the lifetime of the object. *)
  let key_info = foreign "aspell_config_keyinfo" @@ t @-> string @-> returning (opt Key.info)
                                                      
  (** Returns a newly allocated enumeration of all
      the possible objects this config class uses. *)
  let possible_elements =
    let c = foreign "aspell_config_possible_elements" @@ t @-> bool @-> returning Key_enum.t in
    fun t bool -> Key_enum.to_list @@ c t bool

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
  let elements = StringPairEnum.listify @@ foreign "aspell_config_elements" @@ t @-> returning StringPairEnum.t

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
let find_string = foreign "aspell_config_retrieve" @@ t @-> string @-> returning string_opt
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

let find config key =
  let open Key in
  match key_info config key with
  | None -> None
  | Some k ->
    match k.type_ with
    | Kind.Int ->
      let n = find_int config key in
      if n = -1 then None else Some ( Int n )
    | Kind.String ->
      let s = find_string config key in
      ( match s with
        | None -> None
        | Some s -> Some ( String s )
      )
    | Kind.Bool ->
      let b = find_bool config key in
      (match b with
       | -1 -> None
       | 0 -> Some (Bool true)
       | 1 -> Some (Bool false)
       | _ -> assert false
      )
    | Kind.List ->
      let l = String_list.create () in
      let m = String_list.to_mutable_container l  in
      match find_list config key m with
      | false -> None
      | true -> Some ( List ( String_list.to_list l ) )

module Typed = struct
  let find (type a) config (key:a Key.Typed.t ) : a option =
    let open Key.Typed in
    match key.kind with
        | Int ->
      let n = find_int config @@ name key in
      if n = -1 then None else Some n
    | String ->
      let s = find_string config @@ name key in
      ( match s with
        | None -> None
        | Some s -> Some ( s )
      )
    | Bool ->
      let b = find_bool config @@ name key in
      (match b with
       | -1 -> None
       | 0 -> Some (true)
       | 1 -> Some (false)
       | _ -> assert false
      )
    | List ->
      let l = String_list.create () in
      let m = String_list.to_mutable_container l  in
      match find_list config (name key) m with
      | false -> None
      | true -> Some ( String_list.to_list l )


end

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

    

  let (@@@) f x = f x (String.length x) 

module Speller = struct
  type prim
  let prim_t : prim structure typ = structure "AspellSpeller"
  type t =  prim Ctypes.structure Ctypes_static.ptr
  let t = ptr prim_t
  let pre = "aspell_speller_"
  let fp name typ = foreign (pre^name) typ 
  

  let unwrap = foreign "to_aspell_speller" ( Result.c @-> returning t )
 let create config = 
  let c_create = foreign "new_aspell_speller" ( Config.t @-> returning Result.c)
  in Result.convert unwrap @@ c_create config 
  
(** returns  0 if it is not in the dictionary,
    1 if it is, or -1 on error. *)
 let check speller word = 
   let c_check = fp "check" @@ t @-> string @-> int @-> returning int in
   match c_check speller @@@ word with
   | 0 -> Some false
   | 1 -> Some true
   | _ -> None
   
  let suggest s word=
    let c_suggest = fp "suggest" @@ t @-> string @-> int @-> returning Word_list.t in
    Word_list.to_list @@ c_suggest s @@@ word 

  let store_replace =
    let c = fp "store_replacement" @@ t @-> string @-> int @-> string @-> int @-> returning void in
    fun speller ~mispelled ~correction ->
      (c speller @@@ mispelled) @@@ correction

  type auxiliary_dict = Personal | Session
  
  let add_to_session =
    let c= fp "add_to_session"@@  t @-> string @-> int @-> returning void in
    fun t word -> c t @@@ word
      
  let add_to_personal =
    let c = fp "add_to_personal" @@ t @-> string @-> int @-> returning void in
    fun t word -> c t @@@ word

  let add_to = function
    | Session -> add_to_session
    | Personal -> add_to_personal
  
  let delete = foreign "delete_aspell_speller" ( t @-> returning void )
  
  let error_number, error_msg, error = Error.bind pre t
  let config = fp "config" @@ t @-> returning Config.t                                                      
                                                              
  let personal_word_list list=
    let f = fp "personal_word_list" @@ t @-> returning Word_list.t in
    Word_list.to_list @@ f list
  
  let session_word_list list=
    let f = fp "session_word_list" @@ t @-> returning Word_list.t in
    Word_list.to_list @@ f list

  let word_list = function
    | Session -> session_word_list
    | Personal -> personal_word_list
  
  let main_word_list list =
    let f = fp "main_word_list" @@ t @-> returning Word_list.t in
    Word_list.to_list @@ f list
 
  
  let save_all_word_lists = fp "save_all_word_lists" @@ t @-> returning bool
  let clear_session = fp "clear_session" @@ t @-> returning bool
  
end

module Filter = struct
  type prim
  let prim_t : prim structure typ = structure "AspellFilter"
  type t = prim Ctypes.structure Ctypes_static.ptr
  let t = ptr prim_t
  let pre = "aspell_filter_"
  
  let fm name typ= foreign name ( t @-> typ )
  let fmp name typ = fm (pre ^ name) typ
  let delete = fm "delete_aspell_filter" @@ returning void
  let error_number, error_msg, error = Error.bind pre t
      
  let to_ = foreign  "to_aspell_filter" @@ Result.c @-> returning t
end



module Document_checker = struct
  type c_token
  let c_token: c_token structure typ = structure "AspellToken"
  let offset = field c_token "offset" int
  let len = field c_token "len" int
  let () = seal c_token

  type token = { offset: int; len: int }
  let token = view
      ~read:(fun x -> { offset = getf x offset; len = getf x len } )
      ~write:(fun x -> let y = make c_token in
               setf y len x.len; setf y offset x.offset;
               y
             )
      c_token
  
  type prim
  let prim_t : prim structure typ = structure "AspellDocumentChecker"
  type t = prim Ctypes.structure Ctypes_static.ptr    

  let t : t typ = ptr prim_t
  let core_name = "aspell_document_checker"
  let pre = core_name ^ "_"
  let post = "_" ^ core_name
  let fp name typ = foreign (pre^name) typ
  let fpt name typ = foreign (name^post) typ
      
  let delete = fpt "delete" @@ t @-> returning void
  let error_number, error_msg, error = Error.bind pre t                               

  
  let unwrap = fpt "to" @@ Result.c @-> returning t

  (** Creates a new document checker.
    The speller class is expected to last until
    this class is destroyed.
    If config is given it will be used to override
    any relevent options set by this speller class.
    The config class is not once this function is done.
    If filter is given then it will take ownership of
    the filter class and use it to do the filtering.
      You are expected to free the checker when done. *)
  let create speller =
    let c_create = fpt "new" @@ Speller.t @-> returning Result.c in
    Result.convert unwrap @@ c_create speller                                

  
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
let process dc s =
  let c_process = fp "process" @@ t @-> string @-> int @-> returning void in
  c_process dc @@@ s 

(** Returns the next misspelled word in the
    processed string.  If there are no more
    misspelled words, then token.word will be
    NULL and token.size will be 0 *)
let next_misspelling = fp "next_misspelling" @@ t @-> returning token
                                                    
(** Returns the underlying filter class. *)
let filter =  fp "filter" @@ t @-> returning Filter.t

end



module Info = struct
  module Module = struct 
    type s_t
    let c : s_t structure typ = structure "AspellModuleInfo"
    let name = field c "name" string
    let order_num = field c "order_num" double
    let lib_dir = field c "lib_dir" string
    let dict_dirs = field c "dict_dirs" String_list.t
    let dict_exts = field c "dict_exts" String_list.t     
    let () = seal c

    type t = {
      name : string
    ; order_num : float
    ; lib_dir : string
    ; dict_dirs : string list
    ; dict_exts : string list
    }

    let t = view
        ~read:( fun x -> let g n = getf x n in
                { name = g name
                ; order_num = g order_num
                ; lib_dir = g lib_dir
                ; dict_dirs = g dict_dirs
                ; dict_exts = g dict_exts
                }
              )
        ~write:( fun x ->
            let y = make c in
            let s n = setf y n in
            s name x.name
          ; s order_num x.order_num
          ; s lib_dir x.lib_dir
          ; s dict_dirs x.dict_dirs
          ; s dict_exts x.dict_exts
          ; y
          )
        c

  module Enum = Make_enum( struct
      let s_name = "AspellModuleInfoEnumeration"
      let core_name = "aspell_module_info_enumeration"
      type r = t
      let r = opt t
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
    let elements = Enum.listify @@ fp "elements" @@ t @-> returning Enum.t
  end
    
  end
  module Dict = struct
  type s_t
  let c : s_t structure typ = structure "AspellDictInfo"
  (** The Name to identify this dictionary by. *)
  let name = field c "name" string
  (** The language code to identify this dictionary.
      A two letter UPPER-CASE ISO 639 language code
      and an optional two letter ISO 3166 country
      code after a dash or underscore. *)
  let code = field c "code" string
      
  (** Any extra information to distinguish this
      variety of dictionary from other dictionaries
      which may have the same language and size. *)
  let jargon = field c "jargon" string

  let size = field c "size" int

  (** A two char digit code describing the size of
    the dictionary: 10=tiny, 20=really small,
      30=small, 40=med-small, 50=med, 60=med-large,
   70=large, 80=huge, 90=insane.  Please check
   the README in aspell-lang-200?????.tar.bz2 or
   see SCOWL (http://wordlist.sourceforge.net)
      for an example of how these sizes are used. *)
      
  let size_str = field c "size_str" string

  let module_ = field c "module" @@ ptr Module.t

  type t ={
    name : string
  ; code : string
  ; jargon : string
  ; size : int
  ; size_str : string
  ; module_ : Module.t
  }
  
  let t = view
      ~read: (fun x ->
          let g n = getf x n in
          {
            name = g name
          ; code = g code
          ; jargon = g jargon
          ; size = g size
          ; size_str = g size_str
          ; module_ = !@ (g module_)
          }
        )
      ~write:(fun x ->
          let y = make c in
          let s n = setf y n in
          s name x.name
        ; s code x.code
        ; s jargon x.jargon
        ; s size x.size
        ; s size_str x.size_str
        ; s module_ (allocate Module.t x.module_)
        ; y
        )
      c
     

  module Enum = Make_enum( struct
      let s_name = "AspellDictInfoEnumeration"
      let core_name = "aspell_dict_info_enumeration"
      type r = t
      let r = opt t
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


type cache = Decode | Dictionary | Language | Keyboard
let cache = view
    ~read:( function "decode" -> Decode | "dictionary" -> Dictionary | "language" -> Language | "keyboard" -> Keyboard | _ -> assert false)
    ~write:( function Decode -> "decode" | Dictionary -> "dictionary" | Language -> "language" | Keyboard -> "keyboard" )
    string

 (** Reset the global cache(s) so that cache queries will
     create a new object. If existing objects are still in
     use they are not deleted. If which is NULL then all
     caches will be reset. Current caches are "encode",
     "decode", "dictionary", "language", and "keyboard". *)
let reset_cache = foreign "aspell_reset_cache" @@ cache @-> returning bool



