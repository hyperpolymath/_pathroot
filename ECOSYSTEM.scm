;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; ECOSYSTEM.scm — _pathroot (Mustfile)

(ecosystem
  (version "1.0.0")
  (name "_pathroot")
  (type "specification-and-implementation")
  (purpose "Global devtools environment authority with cross-platform discovery")
  (status "v1.0.0-released")
  (languages ("nickel" "ada" "rescript" "rust"))

  (position-in-ecosystem
    "Foundation layer for devtools discovery and environment management. Authority specification for Mustfile ecosystem.")

  (related-projects
    (project (name "must")
             (url "https://github.com/hyperpolymath/must")
             (relationship "implementation-engine"))
    (project (name "nicaug")
             (url "https://github.com/hyperpolymath/nicaug")
             (relationship "configuration-engine"))
    (project (name "mustorch")
             (relationship "orchestrator"))
    (project (name "rhodium-standard-repositories")
             (url "https://github.com/hyperpolymath/rhodium-standard-repositories")
             (relationship "standard")))

  (what-this-is
    "Mustfile specification and reference implementation"
    "Global _pathroot marker system for devtools discovery"
    "Cross-platform environment metadata (_envbase)"
    "22-shell compatibility matrix"
    "Ada TUI for interactive management")

  (what-this-is-not
    "A build system"
    "A package manager"
    "Replacement for shell configuration")

  (ada-tui
    (status "gnat-15.2.1-compatible")
    (bindings "posix-symlinks")
    (build-status "zero-errors"))

  (abi-ffi-standard
    (implementation "universal-idris2-zig")
    (abi-layer "idris2-with-dependent-types")
    (ffi-layer "zig-memory-safe")
    (verification "formal-proofs-included")
    (c-code "none")
    (header-files "none"))

  (opsm-integration
    (relationship "core")
    (description "Path and environment layout for OPSM.")
    (direction "opsm -> _pathroot"))
)
