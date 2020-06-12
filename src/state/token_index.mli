(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2020 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 * Copyright (C) 2020 Maxime Salvagnac
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Create or regenerate token index from sync/ and write sync/token.json.
    This step may take a long time (up to several minutes).  Automatically
    called (once and for all) by [get_tokens] or [add_token] if need be. *)
val create_index : string -> unit Lwt.t

(** Get the list of all tokens. *)
val get_tokens : string -> unit -> Learnocaml_data.Token.t list Lwt.t

(** Add a registered token in the index. *)
val add_token : Learnocaml_data.Token.t -> string -> unit Lwt.t

(** Change the current secret with an empty list of nonce*)
val change_secret : string -> string -> unit Lwt.t

(** Delete all secrets + nonce associated excepted the current secret with its nonces *)
val purge_oauth : string -> unit Lwt.t

(** Related to create_gen *)
val save_path : Learnocaml_data.Token.t -> string -> string

(** Related to create_gen *)
val path : Learnocaml_data.Token.t -> string -> string

(** Create a random token *)
val create_gen : (unit -> Learnocaml_data.Token.t) -> string -> Learnocaml_data.Token.t Lwt.t

(** Get a Moodle user's token, create it if not exist *)
val get_moodle_user_token : string -> string -> Learnocaml_data.Token.t Lwt.t

(** Check OAuth authenticity, return Moodle user's token 
    Don't give the same oauth_consumer_key to differents LTI consumer *)
val process_request : string -> string -> string -> string Lwt.t
