open Yojson
open Lwt
open Learnocaml_data

let token_file = "token.json"
let oauth_file = "oauth.json"
let moodle_file = "moodle_user.json"

(* Unlocked *)
let mutex_token = Lwt_mutex.create ()
let mutex_oauth = Lwt_mutex.create ()
let mutex_moodle = Lwt_mutex.create ()

let cast_list l = `List l
let cast_assoc a = `Assoc a
let cast_list_assoc a = `Assoc [a]

let cast_string (value:string) = (`String value : Yojson.Basic.t)

let string_to_token l = List.map Token.parse l

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
  let data =  l >|= List.map cast_string >|= cast_list in
  data >>= write_file (sync_dir ^ "/" ^  token_file) mutex_token

let create_moodle (sync_dir:string) =
  let data = cast_list_assoc ("DEFAULT", cast_string "DEFAULT") in 
  write_file (sync_dir ^ "/" ^ moodle_file) mutex_moodle data 

(* Copyright https://github.com/astrada/gapi-ocaml 
   Return a secret hexa encoded *)
let gen_secret len =
  let hexa_encode s =
    let transform = Cryptokit.Hexa.encode () in
    transform#put_string s;
    transform#finish;
    transform#get_string
  in
  let secret = hexa_encode @@ Cryptokit.Random.string Cryptokit.Random.secure_rng len in
  Printf.printf "Auto-generated secret : %s\n" secret;
  secret
  

(* Create oauth's log file and fill it with DEFAULT values *)
let create_oauth ?nonce:(n="") (sync_dir:string) secret =
  let data = `Assoc [(secret, cast_list @@ [cast_string n])] in
  write_file (sync_dir ^ "/" ^  oauth_file) mutex_oauth data

let test = write_file 

let create_file (sync_dir : string) = function
  | "index" -> create_index sync_dir
  | "oauth" -> create_oauth sync_dir @@ gen_secret 32
  | "moodle" -> create_moodle sync_dir
  | _ -> Lwt.return_unit

(* if file doesn't exist, create it *)
let get_file name file (sync_dir : string) =
  if Sys.file_exists name then begin
      try 
        Lwt.return @@ Yojson.Basic.from_file name 
    with
          (* Note: this error handling could be adapted later on, to be "more conservative"? (this does not matter now, as the "sync/token.json" file is not critical and can be regenerated) *)
      Json_error _ ->create_file sync_dir file >|= fun () -> Yojson.Basic.from_file name end
  else  
      create_file sync_dir file  >|= fun () -> Yojson.Basic.from_file name 

(* Token list *)
let get_tokens (sync_dir : string) () =
  let json = get_file (sync_dir ^ "/" ^  token_file) "index" sync_dir in
  json >|= Yojson.Basic.Util.to_list >|= List.map Yojson.Basic.Util.to_string >|= string_to_token


let add_token token (sync_dir : string) =
  let token = cast_string @@ Token.to_string token in
  let json_list = get_file (sync_dir ^ "/" ^  token_file) "index" sync_dir >|=  Yojson.Basic.Util.to_list >>= fun l -> Lwt.return @@ token::l in
  json_list >|= cast_list >>= write_file (sync_dir ^ "/" ^  token_file) mutex_token



    (************ OAuth part *******************)


(* add_oauth gen_secret *)
let change_secret secret (sync_dir:string) =
  let assoc = get_file (sync_dir ^ "/" ^  oauth_file) "oauth" sync_dir >|= Yojson.Basic.Util.to_assoc in
  let added = assoc >>= fun l -> Lwt.return @@ (secret, cast_list [cast_string ""]) :: l in 
  added >|= cast_assoc >>= write_file (sync_dir ^ "/" ^  oauth_file) mutex_oauth


let get_first_oauth (sync_dir:string) =
  let assoc = get_file (sync_dir ^ "/" ^  oauth_file) "oauth" sync_dir >|= Yojson.Basic.Util.to_assoc in
  assoc >|= List.hd

let get_current_secret (sync_dir:string) = get_first_oauth sync_dir >|= fst

(* Delete all secrets + nonce associated excepted the current secret *)
let purge_oauth (sync_dir:string) = get_first_oauth sync_dir >|= cast_list_assoc >|= write_file (sync_dir ^ "/" ^  oauth_file) mutex_oauth

(* Add nonce to the current secret *)
let add_nonce (nonce:string) (sync_dir:string) =
  let assoc = get_file (sync_dir ^ "/" ^  oauth_file) "oauth" sync_dir >|= Yojson.Basic.Util.to_assoc in
  assoc >>= fun l ->
  match l with
  | x::reste -> let listex = Yojson.Basic.Util.to_list @@ snd x in
                let nonce = cast_string nonce in
                let assoc = (fst x, cast_list @@ nonce::listex) in
                let data = cast_assoc @@ assoc :: reste in
                write_file (sync_dir ^ "/" ^  oauth_file) mutex_oauth data
  | _ -> create_oauth ~nonce:nonce sync_dir @@ gen_secret 32 (*If nothing, then gen a secret and add the nonce *)


(* Check the nonce for the current secret *)
let check_nonce (nonce:string) (sync_dir:string) =
  let assoc =  get_first_oauth sync_dir >|= snd >|= Yojson.Basic.Util.to_list >|= Yojson.Basic.Util.filter_string in
  assoc >|= List.exists (fun e -> e = nonce) 


(*********** OAuth signature **************)
let oauth_signature_method = ref "HMAC-SHA1"

let rec get_value (key:string) list_args =
  try
    snd @@ List.find (fun (k,_) -> k = key) list_args
  with
    Not_found -> "Error"



let check_oauth_args liste_args =
  (* Traitement de la requête POST *)
  get_value "oauth_signature" liste_args <> "Error"
  && get_value "oauth_timestamp" liste_args <> "Error"
  && get_value "oauth_nonce" liste_args <> "Error"
  && get_value "oauth_version" liste_args <> "Error"
  && get_value "oauth_consumer_key" liste_args <> "Error"
  && get_value "oauth_signature_method" liste_args = !oauth_signature_method



(* Based on gapi-ocaml 
  This function will build a signature by using hmac_sha1 algorithm.*)
let signature_oauth list_args http_method basic_uri secret =
  let pair_encode = (* 1 : encode keys/values *)
    List.map (
        fun (k,v) -> (Netencoding.Url.encode ~plus:false k, Netencoding.Url.encode ~plus:false v))
    @@ List.filter (fun (a,_) -> a <> "oauth_signature") list_args
  in
  let pair_sorted =   (* 2 : Sort by key *)
    List.sort   
      (fun (k1, v1) (k2,v2) ->
        let i = compare k1 k2 in
        if i = 0 then compare v1 v2 else i)
      pair_encode
  in 
  let list_concat =  (* 3 : Form key=value&key2=value2*)
    String.concat "&"
    @@ List.map
         (fun (k,v) -> k ^ "=" ^ v) pair_sorted
  in  
  let signature_base_string =     (* 4 : Add HTTP method and URI *)
    Printf.sprintf "%s&%s&%s" (String.uppercase_ascii http_method) (Netencoding.Url.encode ~plus:false basic_uri) (Netencoding.Url.encode ~plus:false list_concat)
  in
  let signing_key = (Netencoding.Url.encode ~plus:false secret) ^ "&" in  (* 5 : Build signing_key *)
  let encoding =                    
    let hash = Cryptokit.MAC.hmac_sha1 signing_key in
    let _ = hash#add_string signature_base_string in
    let result = hash#result in
    hash#wipe;
    B64.encode result

  in
  encoding 


(* Create_gen things *)
 let path (sync_dir:string) token = Filename.concat sync_dir (Token.to_path token)
 
 let save_path (sync_dir:string) token = Filename.concat (path sync_dir token) "save.json"

 let create_gen rnd (sync_dir:string) =
    let rec aux () =
      let token = rnd () in
      let file = save_path sync_dir token in
      Lwt_utils.mkdir_p ~perm:0o700 (Filename.dirname file) >>= fun () ->
      Lwt.catch (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.Output ~perm:0o700 file
            ~flags:Unix.([O_WRONLY; O_NONBLOCK; O_CREAT; O_EXCL])
            (fun _chan -> Lwt.return token))
      @@ function
      | Unix.Unix_error (Unix.EEXIST, _, _) -> aux ()
      | e -> Lwt.fail e
    in
    aux () >>= fun t -> add_token t sync_dir >|= fun _ -> t


 (* Moodle related *)

 (* Exist *)


 let create_moodle_user (sync_dir:string) (id:string) =
   let assoc = get_file (sync_dir ^ "/" ^ moodle_file) "moodle" sync_dir >|= Yojson.Basic.Util.to_assoc in
   assoc >>= fun l ->
   match l with
   | l ->
      try (* If the user is already registered *)
        Lwt.return @@ Token.parse @@ Yojson.Basic.Util.to_string @@ snd @@ List.find (fun (a,_) -> a = id) l
      with Not_found ->
        let token = create_gen Token.random sync_dir in
        let list =  token >|= Token.to_string >|= cast_string >|= fun t -> (id,t) :: l in
        let data = list >|= cast_assoc in
        data >>= write_file (sync_dir ^ "/" ^  moodle_file) mutex_moodle >>= fun _ -> token

 
 let get_moodle_user_token (sync_dir:string) (id:string) =
   let assoc = get_file (sync_dir ^ "/" ^ moodle_file) "moodle" sync_dir >|= Yojson.Basic.Util.to_assoc in
   let list_string = assoc >|= List.map (fun (a,b) -> (a, Yojson.Basic.Util.to_string b)) in
   let list_token = list_string >|= List.filter (fun (a,_) -> a = id) in
   list_token >>= fun l ->
   match l with
   | x::_ -> Printf.printf "DONNER\n";Lwt.return @@ Token.parse @@ snd x
   | [] -> Printf.printf "CREE\n"; create_moodle_user sync_dir id



(** Don't give the same oauth_consumer_key to differents LTI consumer **)
(* Deal with the request to check OAuth autenticity and return Moodle user's token*)
let process_request req (sync_dir:string) (url:string) =
  let liste_args = Netencoding.Url.dest_url_encoded_parameters req in
  if get_value "oauth_signature_method" liste_args <> !oauth_signature_method then
    Lwt.return "Not implemented"
  else
    begin
      if not (check_oauth_args liste_args) then
        Lwt.return "Missing args"
      else
        check_nonce (get_value "oauth_nonce" liste_args) sync_dir >>= fun b ->
        if b then
          Lwt.return "Nonce already used"
        else
          let check_signature = get_current_secret sync_dir >|= signature_oauth liste_args "post" url in
          check_signature  >>= fun s ->
          if s = get_value "oauth_signature" liste_args then
            create_moodle_user sync_dir ((get_value "user_id" liste_args) ^ (get_value "oauth_consumer_key" liste_args)) >>= fun _ ->
            Lwt.return "OAuth match"
          else
            Lwt.return "Wrong signature"
    end




let test sync_dir () =
  create_moodle_user sync_dir "3COMSUMER" >>= fun t1 ->
  get_moodle_user_token sync_dir "3COMSUMER" >>= fun t2 ->
  Printf.printf "%s == %s\n" (Token.to_string t1) (Token.to_string t2);
  get_tokens sync_dir ()
