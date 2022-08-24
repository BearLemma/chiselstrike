// SPDX-FileCopyrightText: © 2022 ChiselStrike <info@chiselstrike.com>

// this transitively imports all user code
// note that we use file://, but we are not reading anything from actual filesystem: these URLs are passed to
// the `ModuleLoader` in Rust, which reads the code from sources that are ultimately provided by `chisel
// apply`
import { routeMap, topicMap } from "file:///__root.ts";
import run from "chisel:///run.ts";
await run(routeMap, topicMap);