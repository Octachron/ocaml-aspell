open Ctypes
open Foreign
open Aspell.Error


let f name =
  let value = !@ (!@ ( foreign_value ("aerror_" ^name) (ptr info) ) ) in
  value

let other = f "other"
let operation_not_supported = f "operation_not_supported"
let cant_copy = f "cant_copy"
let unimplemented_method = f "unimplemented_method"
let file = f "file"
let cant_open_file = f "cant_open_file"
let cant_read_file = f "cant_read_file"
let cant_write_file = f "cant_write_file"
let invalid_name = f "invalid_name"
let bad_file_format = f "bad_file_format"
let dir = f "dir"
let cant_read_dir = f "cant_read_dir"
let config = f "config"
let unknown_key = f "unknown_key"
let cant_change_value = f "cant_change_value"
let bad_key = f "bad_key"
let bad_value = f "bad_value"
let duplicate = f "duplicate"
let key_not_string = f "key_not_string"
let key_not_int = f "key_not_int"
let key_not_bool = f "key_not_bool"
let key_not_list = f "key_not_list"
let no_value_reset = f "no_value_reset"
let no_value_enable = f "no_value_enable"
let no_value_disable = f "no_value_disable"
let no_value_clear = f "no_value_clear"
let language_related = f "language_related"
let unknown_language = f "unknown_language"
let unknown_soundslike = f "unknown_soundslike"
let language_not_supported = f "language_not_supported"
let no_wordlist_for_lang = f "no_wordlist_for_lang"
let mismatched_language = f "mismatched_language"
let affix = f "affix"
let corrupt_affix = f "corrupt_affix"
let invalid_cond = f "invalid_cond"
let invalid_cond_strip = f "invalid_cond_strip"
let incorrect_encoding = f "incorrect_encoding"
let encoding = f "encoding"
let unknown_encoding = f "unknown_encoding"
let encoding_not_supported = f "encoding_not_supported"
let conversion_not_supported = f "conversion_not_supported"
let pipe = f "pipe"
let cant_create_pipe = f "cant_create_pipe"
let process_died = f "process_died"
let bad_input = f "bad_input"
let invalid_string = f "invalid_string"
let invalid_word = f "invalid_word"
let invalid_affix = f "invalid_affix"
let inapplicable_affix = f "inapplicable_affix"
let unknown_unichar = f "unknown_unichar"
let word_list_flags = f "word_list_flags"
let invalid_flag = f "invalid_flag"
let conflicting_flags = f "conflicting_flags"
let version_control = f "version_control"
let bad_version_string = f "bad_version_string"
let filter = f "filter"
let cant_dlopen_file = f "cant_dlopen_file"
let empty_filter = f "empty_filter"
let no_such_filter = f "no_such_filter"
let confusing_version = f "confusing_version"
let bad_version = f "bad_version"
let identical_option = f "identical_option"
let options_only = f "options_only"
let invalid_option_modifier = f "invalid_option_modifier"
let cant_describe_filter = f "cant_describe_filter"
let filter_mode_file = f "filter_mode_file"
let mode_option_name = f "mode_option_name"
let no_filter_to_option = f "no_filter_to_option"
let bad_mode_key = f "bad_mode_key"
let expect_mode_key = f "expect_mode_key"
let mode_version_requirement = f "mode_version_requirement"
let confusing_mode_version = f "confusing_mode_version"
let bad_mode_version = f "bad_mode_version"
let missing_magic_expression = f "missing_magic_expression"
let empty_file_ext = f "empty_file_ext"
let filter_mode_expand = f "filter_mode_expand"
let unknown_mode = f "unknown_mode"
let mode_extend_expand = f "mode_extend_expand"
let filter_mode_magic = f "filter_mode_magic"
let file_magic_pos = f "file_magic_pos"
let file_magic_range = f "file_magic_range"
let missing_magic = f "missing_magic"
let bad_magic = f "bad_magic"
let expression = f "expression"
let invalid_expression = f "invalid_expression"

