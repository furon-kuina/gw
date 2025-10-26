(lang dune 3.20)

(name gw)

(generate_opam_files true)

(source
 (github furon-kuina/gw))

(authors "furon-kuina")

(maintainers "furon-kuina")

(license MIT License)

(package
 (name gw)
 (synopsis "Git worktree manager")
 (description "A command-line tool for managing git worktrees with per-repository hooks")
 (depends
  ocaml
  dune
  core
  core_unix
  ppx_jane
  otoml)
 (tags
  ("git" "worktree" "cli")))
