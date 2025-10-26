open Core
open Gw

(* let git_worktree_add name = 
  match Core_unix.system "git worktree add " *)

(*
config:
  common:
    base_dir: base directory to create worktrees
  list:
    
  open:
    name: branch to create
    base: base commit-ish to create the branch from
    hook: command to run after creation
  close:
    name: branch to close
*)

let do_open =
  Command.basic ~summary:"Open new worktree"
    (let%map_open.Command branch = anon ("name" %: string)
     and base = flag "base" (optional string) ~doc:"BASE base branch"
     and hook = flag "hook" (optional string) ~doc:"CMD command to run" in
     fun () ->
       (* Load config *)
       let config = Config.load () in

       (* Use flag value or config value or default *)
       let base_branch =
         match base with
         | Some b -> b
         | None -> Option.value config.base ~default:"HEAD"
       in

       let hook_cmd =
         match hook with Some h -> Some h | None -> config.hook_on_open
       in

       (* Create worktree *)
       let cmd =
         sprintf "git worktree add -b %s %s %s" branch branch base_branch
       in
       match Core_unix.system cmd with
       | Ok () ->
           (* Execute hook if present *)
           (match hook_cmd with
           | Some h ->
               printf "Executing hook: %s\n%!" h;
               let hook_with_cd = sprintf "cd %s && %s" branch h in
               ignore (Core_unix.system hook_with_cd)
           | None -> ());
           printf "✓ Worktree created: %s\n" branch;
           printf "cd %s\n%!" branch
       | Error _ -> eprintf "Failed to create worktree\n%!")

let do_close =
  Command.basic ~summary:"Close existing worktree"
    (let%map_open.Command name = anon ("name" %: string) in
     fun () -> printf "Hello, %s\n%!" name)

let do_list =
  Command.basic ~summary:"List existing worktrees"
    (let%map_open.Command name = anon ("name" %: string) in
     fun () -> printf "Hello, %s\n%!" name)

let () =
  Command.group ~summary:"mycli"
    [ ("open", do_open); ("close", do_close); ("list", do_list) ]
  |> Command_unix.run
