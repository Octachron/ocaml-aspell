The ocaml-aspell library is a partial ctype based binding to [gnu aspell spell checker library] (http://aspell.net).


This library revolves mainly around two submodule `Config` and `Speller`. The `Config`
submodule defines an abstract configuration type `Config.t` for aspell dictionary and the associated functions. The `Speller` submodule defines an abstract `Speller.t` type which can be used to check word spelling, suggest nearby spelling or add new word to the session or personal dictionary. 

Note that the binding try to abstract away most of the C idiosyncrasies of the original 
API. Particularly, prefix in function names are replaced by submodule:
For instance, `aspell_config_replace` is mapped to `Aspell.Config.replace`. Abstract types and records in the original C API are mapped to abstract OCaml types and records respectively. Most of the helper classes presents in the C API have been erased in favor to standard OCaml types. 

##Usage
The first step to use the library is to create a configuration object with

``` Ocaml
    open Aspell
    let conf = Config.create ()
```

This configuration object represents a classic key-value map. After creation, the value
are set to default value for every keys. The list of available keys can be retrieved using the `Config.possible_elements` function which returns a list of `Key.Info.t` descriptors:


``` Ocaml
    (* Print the name of available keys *)
     let () = 
      let infos =  Config.possible_elements conf in
      let pp_info info= Printf.printf "key:%s\n" info.name in
      List.iter pp_info infos
``` 

One of the most important variable to be set is the "lang" variable which defines the dictionary. To set a variable to a new value, one can use the `Config.replace` function.
For instance,

```Ocaml
  let () = Config.replace conf "lang" "en_US"
```
will set the default language to use to American English. The language is expected to be the standard two letter ISO 639 language code, with an optional two letter ISO 3166 country code after an underscore. You can set the preferred size via the size option, any extra info via the variety option, and the encoding via the encoding option. Other things you might want to set is the preferred spell checker to use, the search path for dictionaries. To find out the exact name of the dictionary the master option may be examined as well as the master-flags options to see if there were any special flags that were passed on to the module. The module option way also be examined to figure out which speller module was selected, but since there is only one this option will always be the same. See the aspell documentation, for a list of all available options.

It is possible to retrieve the values of the changed key using either the `Config.find` or the `Config.elements` function.

Once the configuration have been set, it is time to create a speller.
The aspell documentation recommand to create one `Speller.t` object by document.
New speller are created using the `Speller.create` function

``` Ocaml
  let spellchecker = match Speller.create conf with
    | `Error msg -> failwith msg
    | ` Ok spell -> spell

```


Once the spellchecker is created you can use the `Speller.check` function to see if a word in the document is correct:
```Ocaml

     let correct = Speller.check spellchecker "enantiomorph"

```


If the word is not correct, then the `Speller.suggest` function can be used to come up with likely replacements:

```Ocaml

    let suggestions = Speller.suggest spellchecker "afect"
    let s () = List.iter (Printf.printf "%s\n") suggestions 

```

Once a replacement is made the `Speller.store_repl` function should be used to communicate the replacement pair back to the spell checker:

```Ocaml
    let () = 
      Speller.store_repl spellchecker ~misspelled ~correction
```

The `Speller.add_to` function can be used if the user decides to add a word to the session or personal dictionary:
```OCaml
     let () = Speller.add_to Speller.Session spellchecker word
(* or *)
     let () = Speller.add_to Speller.Personal spellchecker word
```
It is better to let the spell checker manage these words rather than doing it yourself so that the words have a chance of appearing in the suggestion list.

The list of words in the personal and session dictionary can be checked using
`Speller.word_list` function
```OCaml
    let () = 
      let words = Speller.(word_list Session spellchecker) in
      List.iter (Printf.printf "%s\n") words
```

## Copyright
This documentation is licenced under the GFDL and was largely inspired by the corresponding [gnu aspell documentation] (http://aspell.net/man-html/Through-the-C-API.html)
