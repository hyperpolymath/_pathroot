;; SPDX-License-Identifier: PMPL-1.0
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
  (what-this-is-not "- NOT exempt from RSR compliance"))
