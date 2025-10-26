open Core

type repo_config = {
  base : string option;
  hook_on_open : string option;
}

type t = { default : repo_config; repos : (string * repo_config) list }

let empty_repo_config = { base = None; hook_on_open = None }
let default_config = { default = empty_repo_config; repos = [] }

(* Get the global config file path *)
let global_config_path () =
  let home = Core.Sys.getenv "HOME" |> Option.value ~default:"" in
  sprintf "%s/.config/gw/config.toml" home

(* Normalize git remote URL to a canonical form *)
let normalize_remote_url url =
  String.strip url
  (* git@github.com:user/repo.git -> github.com/user/repo *)
  |> String.substr_replace_all ~pattern:"git@" ~with_:""
  |> String.substr_replace_all ~pattern:":" ~with_:"/"
  |> String.substr_replace_all ~pattern:".git" ~with_:""
  (* https://github.com/user/repo -> github.com/user/repo *)
  |> String.substr_replace_all ~pattern:"https://" ~with_:""
  |> String.substr_replace_all ~pattern:"http://" ~with_:""

(* Get git remote URL for the current repository *)
let get_git_remote () =
  try
    let ic =
      Core_unix.open_process_in "git remote get-url origin 2>/dev/null"
    in
    let result = In_channel.input_line ic in
    let _ = Core_unix.close_process_in ic in
    Option.map result ~f:normalize_remote_url
  with _ -> None

(* Extract repo name from remote URL *)
let get_repo_name remote =
  (* "github.com/user/repo" -> "repo" *)
  String.split remote ~on:'/' |> List.last |> Option.value ~default:remote

(* Parse a repo_config from a TOML table *)
let parse_repo_config toml path =
  let get_string_opt key_path =
    try Some (Otoml.find toml Otoml.get_string key_path) with _ -> None
  in
  {
    base = get_string_opt (path @ [ "base" ]);
    hook_on_open = get_string_opt (path @ [ "hooks"; "on_open" ]);
  }

(* Load config from TOML file *)
let load_from_file path =
  try
    let toml = Otoml.Parser.from_file path in

    (* Parse default section *)
    let default = parse_repo_config toml [ "default" ] in

    (* Parse repos section *)
    let repos =
      try
        let repos_table = Otoml.find toml Otoml.get_table [ "repos" ] in
        let keys = List.map ~f:fst repos_table in
        List.map keys ~f:(fun repo_key ->
            let repo_config = parse_repo_config toml [ "repos"; repo_key ] in
            (repo_key, repo_config))
      with _ -> []
    in

    { default; repos }
  with exn ->
    eprintf "Warning: Failed to parse config file: %s\n%!" (Exn.to_string exn);
    default_config

(* Merge two repo_configs, preferring values from the first *)
let merge_configs c1 c2 =
  {
    base = Option.first_some c1.base c2.base;
    hook_on_open = Option.first_some c1.hook_on_open c2.hook_on_open;
  }

(* Find the config for the current repository *)
let for_current_repo config =
  match get_git_remote () with
  | None -> config.default
  | Some remote_url ->
      let repo_name = get_repo_name remote_url in
      (* Try matching by full remote URL first *)
      let by_url =
        List.Assoc.find config.repos ~equal:String.equal remote_url
      in
      (* Then try matching by repo name *)
      let by_name =
        List.Assoc.find config.repos ~equal:String.equal repo_name
      in
      (* Merge: by_url > by_name > default *)
      let matched =
        match by_url with
        | Some cfg -> cfg
        | None -> Option.value by_name ~default:empty_repo_config
      in
      merge_configs matched config.default

(* Load config for the current repository *)
let load () =
  let config = load_from_file (global_config_path ()) in
  for_current_repo config
