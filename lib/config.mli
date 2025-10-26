(** Configuration for a repository *)
type repo_config = {
  base : string option;  (** Default base branch *)
  hook_on_open : string option;  (** Command to run when opening a worktree *)
}

(** Global configuration with defaults and per-repo overrides *)
type t = {
  default : repo_config;  (** Default configuration *)
  repos : (string * repo_config) list;
      (** Repository-specific configurations, keyed by repo name or remote URL *)
}

(** Load configuration for the current repository.

    This function:
    - Reads from ~/.config/gw/config.toml
    - Detects the current git repository's remote URL
    - Returns repo-specific config if available, otherwise default config
*)
val load : unit -> repo_config

(** Get the path to the global config file *)
val global_config_path : unit -> string
