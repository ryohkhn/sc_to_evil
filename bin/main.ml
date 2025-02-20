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

let username = "ryoh"

let json_members = ["props"; "pageProps"; "__APOLLO_STATE__"]

let sc_endpoint page uni =
  "https://www.senscritique.com/" ^ username ^ "/collection?page=" ^ (string_of_int page) ^ "&universe=" ^ (universe_tag uni) ^ "&action=RATING"

let sc_endpoint_uri page =
  sc_endpoint page MOVIES |> Uri.of_string

let extract_collection_data body =
    let pattern = {|<script id="__NEXT_DATA__" type="application/json">\(.*\)</script><script src=|} in
    let rgx = Str.regexp pattern in
    match Str.search_forward rgx body 0 with
    | exception Not_found -> None
    | _ -> Some (Str.matched_group 1 body)

let fetch_sc_collection_page page =
  sc_endpoint_uri page |> Client.get >>= fun (_, body) ->
    body |> Body.to_string >|= fun body -> body

let parse_json json =
  let json = List.fold_left (fun json member_name -> YojsonBU.member member_name json) json json_members in
  YojsonBU.keys json |> List.iter print_endline;
  ()

let () =
  let body = Lwt_main.run (fetch_sc_collection_page 1) in
  let data = extract_collection_data body in
  let json = match data with
    | Some s -> Some (YojsonB.from_string s)
    | None -> None
  in
  match json with
  | Some json -> parse_json json
  | None -> print_endline "Fetching failed"
