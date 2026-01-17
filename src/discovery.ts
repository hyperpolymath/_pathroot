/**
 * _pathroot Discovery Module
 * Cross-platform discovery of _pathroot marker files
 *
 * SPDX-License-Identifier: PMPL-1.0
 */

import { join } from "@std/path";
import type { DiscoveryResult, Platform } from "./types.ts";

/**
 * Platform-specific search paths for _pathroot
 */
const SEARCH_PATHS: Record<Platform, string[]> = {
  windows: ["C:\\_pathroot", "D:\\_pathroot"],
  wsl: ["/mnt/c/_pathroot", "/mnt/d/_pathroot", "/_pathroot"],
  linux: ["/_pathroot", join(Deno.env.get("HOME") ?? "", ".pathroot")],
  darwin: ["/_pathroot", join(Deno.env.get("HOME") ?? "", ".pathroot")],
};

/**
 * Detect the current platform
 */
export function detectPlatform(): Platform {
  const os = Deno.build.os;

  if (os === "windows") {
    return "windows";
  }

  if (os === "darwin") {
    return "darwin";
  }

  // Check for WSL
  try {
    const versionFile = Deno.readTextFileSync("/proc/version");
    if (versionFile.includes("Microsoft") || versionFile.includes("WSL")) {
      return "wsl";
    }
  } catch {
    // Not WSL or can't read /proc/version
  }

  return "linux";
}

/**
 * Check if a file exists and is readable
 */
async function fileExists(path: string): Promise<boolean> {
  try {
    const stat = await Deno.stat(path);
    return stat.isFile;
  } catch {
    return false;
  }
}

/**
 * Read the devtools root from a _pathroot file
 */
async function readPathrootContent(path: string): Promise<string | null> {
  try {
    const content = await Deno.readTextFile(path);
    return content.trim().replace(/\r?\n/g, "");
  } catch {
    return null;
  }
}

/**
 * Discover the _pathroot file and devtools root
 */
export async function discover(): Promise<DiscoveryResult> {
  const platform = detectPlatform();
  const searchPaths = SEARCH_PATHS[platform];

  for (const pathrootPath of searchPaths) {
    if (await fileExists(pathrootPath)) {
      const devtoolsRoot = await readPathrootContent(pathrootPath);
      if (devtoolsRoot) {
        return {
          found: true,
          pathrootFile: pathrootPath,
          devtoolsRoot,
          platform,
        };
      }
    }
  }

  return {
    found: false,
    platform,
  };
}

/**
 * Validate a devtools root directory
 */
export async function validateDevtoolsRoot(
  root: string,
): Promise<{ valid: boolean; missing: string[] }> {
  const requiredDirs = ["bin", "scripts", "config", "logs", "temp", "tools"];
  const missing: string[] = [];

  for (const dir of requiredDirs) {
    const dirPath = join(root, dir);
    try {
      const stat = await Deno.stat(dirPath);
      if (!stat.isDirectory) {
        missing.push(dir);
      }
    } catch {
      missing.push(dir);
    }
  }

  return {
    valid: missing.length === 0,
    missing,
  };
}
