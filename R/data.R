# Data access: stock universe, sector lookup and cached price history.

NSE_UNIVERSE_URL <- "https://archives.nseindia.com/content/indices/ind_nifty500list.csv"
BROWSER_UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"

# Market proxy: the NIFTY 50 ETF. Yahoo's ^NSEI index has no volume data.
MARKET_PROXY <- "NIFTYBEES.NS"
is_market_symbol <- function(symbol) symbol %in% c(MARKET_PROXY, "^NSEI")

# Built-in NIFTY 50 constituents, used only when the NSE list cannot be downloaded.
# Index membership changes twice a year and companies get renamed or demerged, so
# check this against NSE's current list when you update it (see README).
# Last reviewed: November 2026, against index changes known up to late 2025.
fallback_sector_map <- list(
  "BANKING & FINANCE" = c("HDFCBANK.NS", "ICICIBANK.NS", "SBIN.NS", "AXISBANK.NS", "KOTAKBANK.NS",
                          "BAJFINANCE.NS", "BAJAJFINSV.NS", "SHRIRAMFIN.NS", "JIOFIN.NS"),
  "INSURANCE"  = c("HDFCLIFE.NS", "SBILIFE.NS"),
  "IT"         = c("TCS.NS", "INFY.NS", "HCLTECH.NS", "WIPRO.NS", "TECHM.NS"),
  "AUTO"       = c("TMPV.NS", "MARUTI.NS", "M&M.NS", "BAJAJ-AUTO.NS", "EICHERMOT.NS"),
  "ENERGY"     = c("RELIANCE.NS", "NTPC.NS", "ONGC.NS", "POWERGRID.NS", "COALINDIA.NS"),
  "FMCG"       = c("ITC.NS", "HINDUNILVR.NS", "NESTLEIND.NS", "TATACONSUM.NS"),
  "METALS"     = c("TATASTEEL.NS", "JSWSTEEL.NS", "HINDALCO.NS"),
  "HEALTHCARE" = c("SUNPHARMA.NS", "DRREDDY.NS", "CIPLA.NS", "APOLLOHOSP.NS", "MAXHEALTH.NS"),
  "INFRA"      = c("LT.NS", "ULTRACEMCO.NS", "GRASIM.NS", "ADANIPORTS.NS", "ADANIENT.NS"),
  "DEFENCE"    = c("BEL.NS"),
  "TELECOM"    = c("BHARTIARTL.NS"),
  "CONSUMER"   = c("TITAN.NS", "ASIANPAINT.NS", "TRENT.NS", "ETERNAL.NS", "INDIGO.NS")
)

fallback_universe <- function() {
  data.frame(
    Symbol = unlist(fallback_sector_map, use.names = FALSE),
    Sector = rep(names(fallback_sector_map), lengths(fallback_sector_map)),
    stringsAsFactors = FALSE
  )
}

# Parse NSE's index CSV (columns include Symbol and Industry) into Symbol/Sector.
parse_nse_universe <- function(csv_text) {
  nse <- readr::read_csv(I(csv_text), show_col_types = FALSE)
  if (!all(c("Symbol", "Industry") %in% names(nse))) stop("unexpected NSE CSV columns")
  data.frame(Symbol = paste0(trimws(nse$Symbol), ".NS"),
             Sector = toupper(trimws(nse$Industry)),
             stringsAsFactors = FALSE)
}

# Symbols and sectors to scan: NIFTY 500 from NSE, or the built-in NIFTY 50 list.
# NSE rejects requests without a browser User-Agent, so one is sent.
load_universe <- function(url = NSE_UNIVERSE_URL) {
  uni <- tryCatch({
    resp <- httr::GET(url, httr::user_agent(BROWSER_UA), httr::timeout(20))
    httr::stop_for_status(resp)
    parse_nse_universe(httr::content(resp, as = "text", encoding = "UTF-8"))
  }, error = function(e) {
    message("NSE universe download failed (", conditionMessage(e),
            "); falling back to the built-in NIFTY 50 list.")
    NULL
  })
  source <- "NSE NIFTY 500"
  if (is.null(uni) || nrow(uni) == 0) {
    uni <- fallback_universe()
    source <- "built-in NIFTY 50 fallback"
  }
  uni <- uni[!duplicated(uni$Symbol), ]
  uni <- uni[order(uni$Symbol), ]
  attr(uni, "source") <- source
  uni
}

# Named lookup vector: symbol -> sector.
make_sector_lookup <- function(universe) stats::setNames(universe$Sector, universe$Symbol)

get_sector_name <- function(symbols, lookup) {
  out <- unname(lookup[symbols])
  out[is.na(out)] <- "OTHER"
  out
}

# --- Price history cache ---
# A disk cache is shared between the Shiny process and the parallel scan workers.
# Entries expire after CACHE_TTL_SECS so a long-running session does not serve
# stale prices. Failed downloads are cached too, so a dead ticker is not retried
# on every scan.
CACHE_TTL_SECS <- 15 * 60

new_stock_cache <- function(dir = file.path(tempdir(), "scout_cache")) {
  cachem::cache_disk(dir = dir, max_age = CACHE_TTL_SECS)
}

# cachem keys may only contain [a-z0-9_-]; tickers like "M&M.NS" don't, so hex-encode.
cache_key <- function(symbol, days_back) {
  paste(sprintf("%02x", as.integer(charToRaw(paste0(symbol, "|", days_back)))), collapse = "")
}

# Download daily OHLCV from Yahoo, retrying with backoff on transient errors
# (Yahoo rate-limits bursts of requests). Returns NULL on failure.
fetch_history <- function(symbol, days_back, tries = 3) {
  fetch_sym <- if (symbol == "^NSEI") MARKET_PROXY else symbol
  for (attempt in seq_len(tries)) {
    res <- tryCatch(
      quantmod::getSymbols(fetch_sym, src = "yahoo", auto.assign = FALSE,
                           from = Sys.Date() - days_back, warnings = FALSE),
      error = function(e) e
    )
    if (!inherits(res, "error")) return(zoo::na.locf(res, na.rm = TRUE))
    # A missing or delisted ticker will not succeed on retry.
    if (grepl("404|not found|no data", conditionMessage(res), ignore.case = TRUE)) break
    if (attempt < tries) Sys.sleep(2^(attempt - 1) + stats::runif(1))
  }
  NULL
}

get_cached_stock_data <- function(symbol, days_back, cache) {
  key <- cache_key(symbol, days_back)
  cached <- cache$get(key)
  if (!cachem::is.key_missing(cached) &&
      difftime(Sys.time(), cached$time, units = "secs") < CACHE_TTL_SECS) {
    return(cached$data)
  }
  data <- fetch_history(symbol, days_back)
  cache$set(key, list(time = Sys.time(), data = data))
  data
}

# Fetch + analyse one symbol.
get_technical_analysis <- function(symbol, timeframe, cache, market_score = NULL,
                                   in_top_sector = FALSE) {
  data <- tryCatch(get_cached_stock_data(symbol, days_back_for(timeframe), cache),
                   error = function(e) NULL)
  tryCatch(
    analyze_data(data, symbol, timeframe, market_score, in_top_sector,
                 is_market = is_market_symbol(symbol)),
    error = function(e) NULL
  )
}

# Scan a chunk of symbols (runs inside a parallel worker).
# Returns a data.frame of candidates passing the liquidity pre-filter, or NULL.
scan_symbols <- function(symbols, timeframe, market_score, cache) {
  rows <- lapply(symbols, function(sym) {
    if (is_market_symbol(sym)) return(NULL)
    res <- get_technical_analysis(sym, timeframe, cache, market_score = market_score)
    if (is.null(res) || !(res$vol_ratio > 0.5 && res$price > 20)) return(NULL)
    data.frame(Symbol = sym, Price = res$price, Score = res$score,
               VolRatio = res$vol_ratio, stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}
