
module Key :   sig
  module Kind: sig
    type t = String | Int | Bool | List
    val show : t -> string
  end
  
    type value =
        String of string
      | Int of int
      | Bool of bool
      | List of string list
            
    val show_value : value -> string

    type info = {
      (** The name of the key *)
      name : string;
      (** Key type *)
      type_ : Kind.t;
      (** Default value *) 
      def : string;
      (** Brief description *)
      desc : string;
      (** Flags? *)
      flags : int;
      (** Other data?? *)
      other_data : int;
    }

    val show : info -> string

    module Typed : sig
      type 'a kind =
        | String: string kind
        | Int: int kind
        | Bool: bool kind
        | List: string list kind

      type 'a t
      val info : 'a t -> info
      exception Key_type_error
      val convert: 'a kind -> info -> 'a t
      
    end

      
  end

module Error :
  sig
    type info = { mesg : string; num_parms : int; parms : string array; }
    val info: info list Ctypes.typ
    val show : info -> string
    type t = { mesg : string; err : info list; }
  end

module Config :
  sig
    type t

    val create : unit -> t
    val clone : t -> t
    val with_ : (t -> 'a) -> 'a
    val assign : t -> t -> unit

    val error_number : t -> Unsigned.uint
    val error_msg : t -> string
    val error : t -> Error.t

    (** Sets extra keys which this config class should accept *)
    val set_extra : t -> Key.info list -> unit

    (** Returns the Key.Info.t descriptor for the
        corresponding key or returns None and sets
        error_num to PERROR_UNKNOWN_KEY if the key is
        not valid *)
    val key_info : t -> string -> Key.info option

    (** Returns a list of all possible keys available within the configuration. *)
    val possible_elements : t -> bool -> Key.info list
    val get_default : t -> string -> string option

    (** Returns the list of key/value pairs excepted the ones set 
        to their default values *)
    val elements : t -> (string * string) list
        
    val replace : t -> string -> string -> bool
    val remove : t -> string -> bool
    val mem : t -> string -> bool
    (** Retrieve the value of a key.*)
    val find : t -> string -> Key.value option

    (** Experimental strongly typed key function *)
    module Typed : sig 
      val find : t -> 'a Key.Typed.t -> 'a option
    end
  end
  
module Result :
  sig
    type 'a t = [ `Error of string | `Ok of 'a ]
  end

module Speller :
  sig
    type t

    val create : Config.t -> t Result.t

    val config : t -> Config.t
    
    val check : t -> string -> bool option
    val suggest : t -> string -> string list

    val error_number : t -> Unsigned.uint
    val error_msg : t -> string
    val error : t -> Error.t

    (** Store a replacement for a mispelled word *)
    val store_replace : t -> mispelled:string -> correction:string -> unit

    (** Auxiliary dictionary type: Either session or personal *)
    type auxiliary_dict = Personal | Session
                          
    (** Add a new word in the session or personal dictionary *)
    val add_to : auxiliary_dict -> t -> string -> unit

    (** Retrieve the list of word in the session or personal dictionary *)
    val word_list : auxiliary_dict ->  t -> string list

    (** Raise an segfault from inside libaspell ... 
    val main_word_list : t -> string list 
    *)

    val save_all_word_lists : t -> bool
    val clear_session : t -> bool
  end
module Filter :
  sig
    type t

    val delete : t -> unit
    val error_number : t -> Unsigned.uint
    val error_msg : t -> string
    val error : t -> Error.t
  end
module Document_checker :
  sig
    type token = { offset : int; len : int; }
    type t

    val create : Speller.t  -> t Result.t
    
    val error_number : t -> Unsigned.uint
    val error_msg : t -> string
    val error : t -> Error.t

    (** Reset the internal state of the filter.
        Should be called whenever a new document is
        being filtered. *)
    val reset : t -> unit
      
    (** Process a string.
        The string passed in should only be split on
        white space characters.  Furthermore, between
        calls to reset, each string should be passed
        in exactly once and in the order they appeared
        in the document.  Passing in strings out of
        order, skipping strings or passing them in
        more than once may lead to undefined results. *)
    val process : t -> string -> unit
      
    (** Returns the next misspelled word in the
        processed string.  If there are no more
        misspelled words, then token.size will be 0 *)
    val next_misspelling : t -> token
    val filter : t -> Filter.t
  end
module Info :
  sig
    module Module :
      sig
        type t = {
          name : string
        ; order_num : float
        ; lib_dir : string
        ; dict_dirs : string list
        ; dict_exts : string list
        }
      end
    module Dict :
    sig
      type t ={
        name : string
      ; code : string
      ; jargon : string
      ; size : int
      ; size_str : string
      ; module_ : Module.t
      }
    end
  end
type cache = Decode | Dictionary | Language | Keyboard
val cache : cache Ctypes.typ
    
(** Reset the global cache(s) so that cache queries will
     create a new object. If existing objects are still in
     use they are not deleted. *)
val reset_cache : cache -> bool
