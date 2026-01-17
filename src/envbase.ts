/**
 * _pathroot Envbase Module
 * Handles _envbase JSON parsing and manipulation
 *
 * SPDX-License-Identifier: PMPL-1.0
 */

import { join } from "@std/path";
import type { EnvBase, Platform } from "./types.ts";
import { detectPlatform } from "./discovery.ts";

/**
 * Get the path to the _envbase file
 */
export function getEnvbasePath(devtoolsRoot: string): string {
  return join(devtoolsRoot, "_envbase");
}

/**
 * Load _envbase from disk
 */
export async function loadEnvbase(devtoolsRoot: string): Promise<EnvBase | null> {
  const path = getEnvbasePath(devtoolsRoot);

  try {
    const content = await Deno.readTextFile(path);
    return JSON.parse(content) as EnvBase;
  } catch {
    return null;
  }
}

/**
 * Save _envbase to disk
 */
export async function saveEnvbase(devtoolsRoot: string, envbase: EnvBase): Promise<void> {
  const path = getEnvbasePath(devtoolsRoot);
  const content = JSON.stringify(envbase, null, 2) + "\n";
  await Deno.writeTextFile(path, content);
}

/**
 * Create a default _envbase configuration
 */
export function createDefaultEnvbase(platform?: Platform): EnvBase {
  return {
    env: "devtools",
    profile: "default",
    platform: platform ?? detectPlatform(),
    version: "0.1.0",
    created: new Date().toISOString(),
  };
}

/**
 * Switch to a different profile
 */
export async function switchProfile(devtoolsRoot: string, profileName: string): Promise<void> {
  const envbase = await loadEnvbase(devtoolsRoot);
  if (!envbase) {
    throw new Error("_envbase not found");
  }

  envbase.profile = profileName;
  await saveEnvbase(devtoolsRoot, envbase);
}

/**
 * Update _envbase with partial values
 */
export async function updateEnvbase(
  devtoolsRoot: string,
  updates: Partial<EnvBase>,
): Promise<EnvBase> {
  const existing = await loadEnvbase(devtoolsRoot);
  const envbase: EnvBase = {
    ...createDefaultEnvbase(),
    ...existing,
    ...updates,
  };

  await saveEnvbase(devtoolsRoot, envbase);
  return envbase;
}
