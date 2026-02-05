/**
 * Deno API bindings for ReScript
 * External bindings to Deno runtime APIs
 *
 * SPDX-License-Identifier: PMPL-1.0-or-later
 */

// Deno.build
type build = {os: string}

@module("Deno") @scope("build") external build: build = "build"

// Deno.env
module Env = {
  @module("Deno") @scope("env") external get: string => option<string> = "get"
}

// Deno.stat
type fileInfo = {
  isFile: bool,
  isDirectory: bool,
}

@module("Deno") external stat: string => promise<fileInfo> = "stat"

// Deno.readTextFile
@module("Deno") external readTextFile: string => promise<string> = "readTextFile"

// Deno.writeTextFile
@module("Deno") external writeTextFile: (string, string) => promise<unit> = "writeTextFile"

// Deno.readTextFileSync
@module("Deno") external readTextFileSync: string => string = "readTextFileSync"

// Deno.exit
@module("Deno") external exit: int => unit = "exit"

// @std/path bindings
@module("@std/path") external join: (string, string) => string = "join"
@module("@std/path") external joinMany: array<string> => string = "join"
