# Startup: packages, modules, config and shared state. Shiny sources this file
# before ui.R and server.R. R/_disable_autoload.R stops Shiny from sourcing R/
# a second time, so the modules load exactly once, from here.

library(shiny)
library(bslib)
library(quantmod)
library(future)
library(promises)

for (f in sort(list.files("R", pattern = "^[^_].*\\.[rR]$", full.names = TRUE))) source(f, encoding = "UTF-8")

# --- Config (.env uses the same KEY=value format as .Renviron) ---
if (file.exists(".env")) readRenviron(".env")
OPENROUTER_API_KEY <- Sys.getenv("OPENROUTER_API_KEY")
OPENROUTER_MODEL   <- Sys.getenv("OPENROUTER_MODEL", DEFAULT_AI_MODEL)

# --- Parallel workers ---
# A flat pool: the market scan splits the universe into one chunk per worker and
# runs each chunk as its own future. Capped at 8 to stay under Yahoo's rate limits.
n_workers <- as.integer(Sys.getenv("SCOUT_WORKERS", max(1, min(8, parallelly::availableCores() - 1))))
plan(multisession, workers = n_workers)

# --- Shared state ---
stock_cache   <- new_stock_cache()
universe      <- load_universe()
sector_lookup <- make_sector_lookup(universe)
live_scan_list <- universe$Symbol
message("Universe: ", length(live_scan_list), " symbols (", attr(universe, "source"), ")")
