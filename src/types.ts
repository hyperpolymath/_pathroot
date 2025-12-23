/**
 * _pathroot TypeScript Type Definitions
 * Cross-platform devtools environment management
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

/**
 * Platform identifiers
 */
export type Platform = "windows" | "linux" | "darwin" | "wsl";

/**
 * Environment metadata stored in _envbase
 */
export interface EnvBase {
  /** Environment name (e.g., "devtools") */
  env: string;
  /** Active profile name */
  profile: string;
  /** Platform identifier */
  platform: Platform;
  /** Version of the _pathroot system */
  version?: string;
  /** ISO timestamp when created */
  created?: string;
  /** TUI-specific settings */
  tui?: TuiConfig;
}

/**
 * TUI configuration options
 */
export interface TuiConfig {
  /** Color theme */
  theme?: "dark" | "light";
  /** Number of log lines to display */
  log_lines?: number;
  /** Auto-validate paths on startup */
  auto_validate?: boolean;
}

/**
 * Discovery result from finding _pathroot
 */
export interface DiscoveryResult {
  /** Whether _pathroot was found */
  found: boolean;
  /** Path to _pathroot file */
  pathrootFile?: string;
  /** Path to devtools root directory */
  devtoolsRoot?: string;
  /** Detected platform */
  platform: Platform;
}

/**
 * Validation result for environment checks
 */
export interface ValidationResult {
  /** Check name */
  check: string;
  /** Result status */
  status: "pass" | "warn" | "fail";
  /** Human-readable message */
  message: string;
}

/**
 * Transaction command for TUI protocol
 */
export interface TransactionCommand {
  /** Command type */
  type:
    | "QUERY:ENV"
    | "SET:PROFILE"
    | "LINK"
    | "AUDIT:LINKS"
    | "PATH:ADD"
    | "PATH:REMOVE";
  /** Command parameters */
  params?: string[];
}

/**
 * Transaction response from TUI
 */
export interface TransactionResponse {
  /** Response status */
  status: "ok" | "error" | "warning";
  /** Command that was executed */
  command: string;
  /** Result data */
  result: Record<string, unknown>;
}

/**
 * Symbolic link definition
 */
export interface SymLink {
  /** Source path (target of the link) */
  source: string;
  /** Destination path (the link itself) */
  destination: string;
  /** Link status */
  status?: "valid" | "broken" | "missing";
}

/**
 * PATH entry with validation
 */
export interface PathEntry {
  /** Path string */
  path: string;
  /** Whether path exists */
  exists: boolean;
  /** Index in PATH */
  index: number;
}
