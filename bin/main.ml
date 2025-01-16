open Cohttp_lwt_unix
open Cohttp_lwt
open Lwt
open Str

module YojsonB = Yojson.Basic
module YojsonBU = Yojson.Basic.Util

type universe =
  | MOVIES
  | TV_SHOWS
  | GAMES
  | BDS

let universe_tag = function
  | MOVIES -> "1"
  | TV_SHOWS -> "4"
  | GAMES -> "3"
  | BDS -> "6"

let sc_endpoint page uni =
  "https://www.senscritique.com/ryoh/collection?page=" ^ (string_of_int page) ^ "&universe=" ^ (universe_tag uni) ^ "&action=RATING"

let sc_endpoint_uri page =
  sc_endpoint page MOVIES |> Uri.of_string

let extract_collection_data body =
    let pattern = {|<script id="__NEXT_DATA__" type="application/json">\(.*\)</script><script src=|} in
    let rgx = Str.regexp pattern in
    match Str.search_forward rgx body 0 with
    | _i -> Some (Str.matched_group 1 body)
    | exception Not_found -> None

let fetch_sc_collection_page page =
  sc_endpoint_uri page |> Client.get >>= fun (_, body) ->
    body |> Body.to_string >|= fun body -> body

let () =
  let body = Lwt_main.run (fetch_sc_collection_page 1) in
  let data = extract_collection_data body in
  match data with
  | Some data -> print_endline data
  | None -> print_endline "No data found"
