open Yojson
open Lwt
open Learnocaml_data


let token_file = "sync/token.json"
let oauth_file = "sync/oauth.json"

(* Unlocked *)
let mutex_token = Lwt_mutex.create ()
let mutex_oauth = Lwt_mutex.create ()

let cast_list l = `List l
let cast_assoc a = `Assoc a
let cast_list_assoc a = `Assoc [a]

let string_to_json (value:string) = (`String value : Yojson.Basic.t)

let token_to_string l = List.map (fun t -> Token.to_string t) l

let string_to_token l = List.map (fun t -> Token.parse t) l

let get (sync_dir : string) () =
      let base = sync_dir in
      let ( / ) dir f = if dir = "" then f else Filename.concat dir f in
      let rec scan f d acc =
        let rec aux s acc =
          Lwt.catch (fun () ->
              Lwt_stream.get s >>= function
              | Some ("." | "..") -> aux s acc
              | Some x -> scan f (d / x) acc >>= aux s
              | None -> Lwt.return acc)
          @@ function
          | Unix.Unix_error (Unix.ENOTDIR, _, _) -> f d acc
          | Unix.Unix_error _ -> Lwt.return acc
          | e -> Lwt.fail e
        in
        aux (Lwt_unix.files_of_directory (base / d)) acc
      in
      scan (fun d acc ->
          let d =
            if Filename.basename d = "save.json" then Filename.dirname d
            else d
          in
          let stok = String.map (function '/' | '\\' -> '-' | c -> c) d in
          if Token.check stok then
            Lwt.return (stok :: acc)
          else
            Lwt.return acc
        ) "" []


let write_file file mutex data =
  Lwt_mutex.lock mutex >|= fun () ->
  let oo = open_out file in
  Yojson.Basic.pretty_to_channel oo data;
  close_out oo;
  Lwt_mutex.unlock mutex

let create_index (sync_dir : string) =
  let l = get sync_dir () in
  let data =  l >|= List.map string_to_json >|= cast_list in
  data >>= write_file token_file mutex_token


(* Create oauth's log file and fill it with DEFAULT values *)
let create_oauth =
  let data = `Assoc [("DEFAULT", cast_list @@ List.map string_to_json ["NONCE1";"NONCE2";"NONCE3"])] in
  write_file oauth_file mutex_oauth data

let test = write_file 

let create_file (sync_dir : string) = function
  | "index" -> create_index sync_dir
  | "oauth" -> create_oauth 
  | _ -> Lwt.return_unit

(* if file doesn't exist, create it *)
let get_file name file (sync_dir : string) =
  if Sys.file_exists name then begin
      try
          Lwt.return @@ Yojson.Basic.from_file name
    with
          (* Note: this error handling could be adapted later on, to be "more conservative"? (this does not matter now, as the "sync/token.json" file is not critical and can be regenerated) *)
      Json_error _ -> create_file file sync_dir >|= fun () -> Yojson.Basic.from_file name end
  else
      create_file file sync_dir  >|= fun () -> Yojson.Basic.from_file name


(* Token list *)
let get_tokens (sync_dir : string) () =
  let json = get_file token_file "index" sync_dir in
  json >|= Yojson.Basic.Util.to_list >|= List.map Yojson.Basic.Util.to_string >|= string_to_token


let add_token token (sync_dir : string) =
  let token = string_to_json @@ Token.to_string token in
  let json_list = get_file token_file "index" sync_dir >|=  Yojson.Basic.Util.to_list >>= fun l -> Lwt.return @@ token::l in
  json_list >|= cast_list >>= write_file token_file mutex_token



    (************ OAuth part *******************)

(** TODO **)
let gen_secret = "secret"

(* add_oauth gen_secret *)
let change_secret secret (sync_dir:string) =
  let assoc = get_file oauth_file "oauth" sync_dir >|= Yojson.Basic.Util.to_assoc in
  let added = assoc >>= fun l -> Lwt.return @@ (secret, cast_list []) :: l in 
  added >|= cast_assoc >>= write_file oauth_file mutex_oauth


let get_first_oauth (sync_dir:string) =
  let assoc = get_file oauth_file "oauth" sync_dir >|= Yojson.Basic.Util.to_assoc in
  assoc >|= List.hd

let get_current_secret (sync_dir:string) = get_first_oauth sync_dir >|= fst

(* Delete all secrets + nonce associated excepted the current secret *)
let purge_oauth (sync_dir:string) = get_first_oauth sync_dir >|= cast_list_assoc >|= write_file oauth_file mutex_oauth

(*let add_nonce nonce (sync_dir:string) =
  let assoc = get_file oauth_file "oauth" sync_dir >|= Yojson.Basic.Util.to_assoc in
  let ajout assoc = assoc >|= fun l ->
  match l with
  |x::l -> Lwt.return @@ (nonce :: (Yojson.Basic.Util.to_list @@ snd x))::l
  | [] -> l in
  ajout assoc*)


  (* Vérifier nonce *)

  (* Poc : vérification de la signature et traitement *)

let test sync_dir () =
  (* purge_oauth sync_dir;*)
  get_tokens sync_dir ()
