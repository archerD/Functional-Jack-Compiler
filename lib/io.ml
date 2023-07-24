(* open Base *)
open Stdio
(* function below from https://gist.github.com/lindig/be55f453026c65e761f4e7012f8ab9b5 *)
(*
 * contained in [dir]. Each file is a path starting with [dir].
 *)
let dir_contents dir =
  let rec loop result = function
    | f::fs when Sys.is_directory f ->
          Sys.readdir f
          |> Array.to_list
          |> List.map (Filename.concat f)
          |> List.append fs
          |> loop result
    | f::fs -> loop (f::result) fs
    | []    -> result
  in
    loop [] [dir]

let filter_extension extension file =
    Filename.check_suffix file extension

    (* new_extension should include the dot, i.e., '.vm' instead of 'vm' *)
let change_extension new_extension file =
    let prefix = Filename.remove_extension file
    in prefix ^ new_extension

let map_over_dir_output name_transformer file_transformer dir_name =
    let files = dir_contents dir_name in
    let new_names = List.map name_transformer files in
    List.iter2 (fun file new_file ->
        Out_channel.write_all new_file ~data:(In_channel.read_all file |> file_transformer))
        files new_names
let output_to_file file contents =
    Out_channel.write_all file ~data:contents
let read_file file = In_channel.read_all file

