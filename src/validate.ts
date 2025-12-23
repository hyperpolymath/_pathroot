/**
 * _pathroot Validation CLI
 * Validates _pathroot environment configuration
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

import { discover, validateDevtoolsRoot } from "./discovery.ts";
import { loadEnvbase, getEnvbasePath } from "./envbase.ts";
import type { ValidationResult } from "./types.ts";

/**
 * ANSI color codes
 */
const colors = {
  reset: "\x1b[0m",
  green: "\x1b[32m",
  yellow: "\x1b[33m",
  red: "\x1b[31m",
  cyan: "\x1b[36m",
};

/**
 * Format a validation result for display
 */
function formatResult(result: ValidationResult): string {
  const statusColors = {
    pass: colors.green,
    warn: colors.yellow,
    fail: colors.red,
  };

  const statusSymbols = {
    pass: "✓",
    warn: "⚠",
    fail: "✗",
  };

  const color = statusColors[result.status];
  const symbol = statusSymbols[result.status];

  return `${color}[${symbol}]${colors.reset} ${result.check}: ${result.message}`;
}

/**
 * Run all validation checks
 */
async function runValidation(): Promise<ValidationResult[]> {
  const results: ValidationResult[] = [];

  // Discover _pathroot
  const discovery = await discover();

  results.push({
    check: "_pathroot exists",
    status: discovery.found ? "pass" : "fail",
    message: discovery.found
      ? `Found at ${discovery.pathrootFile}`
      : "Not found in standard locations",
  });

  if (!discovery.found || !discovery.devtoolsRoot) {
    return results;
  }

  // Check devtools root
  try {
    const stat = await Deno.stat(discovery.devtoolsRoot);
    results.push({
      check: "devtools root exists",
      status: stat.isDirectory ? "pass" : "fail",
      message: discovery.devtoolsRoot,
    });
  } catch {
    results.push({
      check: "devtools root exists",
      status: "fail",
      message: `Directory not found: ${discovery.devtoolsRoot}`,
    });
    return results;
  }

  // Validate directory structure
  const dirValidation = await validateDevtoolsRoot(discovery.devtoolsRoot);
  if (dirValidation.valid) {
    results.push({
      check: "directory structure",
      status: "pass",
      message: "All required directories present",
    });
  } else {
    results.push({
      check: "directory structure",
      status: "warn",
      message: `Missing: ${dirValidation.missing.join(", ")}`,
    });
  }

  // Check _envbase
  const envbase = await loadEnvbase(discovery.devtoolsRoot);
  if (envbase) {
    results.push({
      check: "_envbase exists",
      status: "pass",
      message: `env=${envbase.env}, profile=${envbase.profile}`,
    });

    // Validate required fields
    if (envbase.env && envbase.profile && envbase.platform) {
      results.push({
        check: "_envbase schema",
        status: "pass",
        message: "All required fields present",
      });
    } else {
      results.push({
        check: "_envbase schema",
        status: "warn",
        message: "Missing required fields",
      });
    }
  } else {
    results.push({
      check: "_envbase exists",
      status: "warn",
      message: `Not found at ${getEnvbasePath(discovery.devtoolsRoot)}`,
    });
  }

  // Check platform detection
  results.push({
    check: "platform detection",
    status: "pass",
    message: discovery.platform,
  });

  return results;
}

/**
 * Main entry point
 */
async function main(): Promise<void> {
  console.log(`${colors.cyan}======================================${colors.reset}`);
  console.log(`${colors.cyan} _pathroot Validation${colors.reset}`);
  console.log(`${colors.cyan}======================================${colors.reset}`);
  console.log();

  const results = await runValidation();

  for (const result of results) {
    console.log(formatResult(result));
  }

  console.log();

  const failures = results.filter((r) => r.status === "fail").length;
  const warnings = results.filter((r) => r.status === "warn").length;

  console.log(`Summary: ${failures} failures, ${warnings} warnings`);

  if (failures > 0) {
    Deno.exit(1);
  }
}

main();
