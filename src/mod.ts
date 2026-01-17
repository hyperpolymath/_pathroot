/**
 * _pathroot - Modular Devtools Environment Management
 *
 * A cross-platform system for managing development tool environments
 * with discoverable paths, environment metadata, and automation support.
 *
 * SPDX-License-Identifier: PMPL-1.0
 *
 * @example
 * ```ts
 * import { discover, loadEnvbase } from "@pathroot/tools";
 *
 * const result = await discover();
 * if (result.found) {
 *   const envbase = await loadEnvbase(result.devtoolsRoot!);
 *   console.log(`Profile: ${envbase?.profile}`);
 * }
 * ```
 *
 * @module
 */

export * from "./types.ts";
export * from "./discovery.ts";
export * from "./envbase.ts";
