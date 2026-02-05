;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; META.scm — _pathroot

(define-module (pathroot meta)
  #:export (architecture-decisions development-practices design-rationale))

(define architecture-decisions
  '((adr-001
     (title . "RSR Compliance")
     (status . "accepted")
     (date . "2025-12-15")
     (context . "Rhodium Standard Repository guidelines")
     (decision . "Follow RSR for consistency, security, and maintainability")
     (consequences . ("RSR Gold target" "SHA-pinned actions" "SPDX headers" "Multi-platform CI")))
    (adr-002
     (title . "Universal ABI/FFI Standard (Idris2 + Zig)")
     (status . "accepted")
     (date . "2026-02-05")
     (context . "GNAT 15.2.1 removed deprecated POSIX functions; need formal verification")
     (decision . "Adopt universal Idris2 ABI + Zig FFI standard for POSIX symlink operations")
     (consequences . ("Formal verification via dependent types"
                      "Memory-safe Zig implementation"
                      "Zero-overhead FFI abstraction"
                      "No C code, no header files"
                      "Provable correctness for critical operations")))))

(define development-practices
  '((code-style (languages . ("Ada" "ReScript" "Rust" "Nickel" "Idris2" "Zig"))
                (formatter . "auto-detect")
                (linter . "auto-detect"))
    (security (sast . "CodeQL")
              (credentials . "env vars only")
              (formal-verification . "Idris2 dependent types"))
    (testing (coverage-minimum . 70)
             (unit-tests . "Zig built-in test framework")
             (integration-tests . "Ada TUI transaction mode"))
    (versioning (scheme . "SemVer 2.0.0"))))

(define design-rationale
  '((why-rsr "RSR ensures consistency, security, and maintainability.")
    (why-idris2-abi "Dependent types prove interface correctness at compile-time")
    (why-zig-ffi "Memory-safe C ABI compatibility without overhead")
    (why-no-c "Pure verified implementations eliminate C undefined behavior")
    (why-pathroot "Global marker solves 'where are my devtools?' problem")))

(define opsm-link "OPSM link: Path and environment layout for OPSM.")
