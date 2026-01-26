;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; ECOSYSTEM.scm — rsr-template-repo

(ecosystem
  (version "1.0.0")
  (name "rsr-template-repo")
  (type "project")
  (purpose "// Badges")

  (position-in-ecosystem
    "Part of hyperpolymath ecosystem. Follows RSR guidelines.")

  (related-projects
    (project (name "rhodium-standard-repositories")
             (url "https://github.com/hyperpolymath/rhodium-standard-repositories")
             (relationship "standard")))

  (what-this-is "// Badges")
  (what-this-is-not "- NOT exempt from RSR compliance")
  (opsm-integration
    (relationship "core")
    (description "Path and environment layout for OPSM.")
    (direction "opsm -> _pathroot"))
)
