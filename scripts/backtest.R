# Backtest the scoring rules: do higher scores lead to better forward returns?
#
# Usage (from the repo root):
#   Rscript scripts/backtest.R [daily|weekly] [years] [universe]
#     universe: "nifty50" (built-in list, default) or "nse" (NIFTY 500 from NSE)
#
# For every stock and every historical bar, it computes the score the app would
# have shown that day (including the market-context penalty) and the return over
# the next 5/10/20 bars, then averages by verdict band. If the higher bands don't
# beat the lower ones, the weights in R/scoring.R need work.
# Caveats: survivorship bias (today's index members only), no costs or slippage,
# and the leading-sector bonus is not modelled.

suppressPackageStartupMessages({ library(quantmod) })
for (f in c("R/scoring.R", "R/data.R", "R/backtest.R")) source(f, encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
timeframe <- if (length(args) >= 1) args[1] else "daily"
years     <- if (length(args) >= 2) as.numeric(args[2]) else 3
uni_kind  <- if (length(args) >= 3) args[3] else "nifty50"
stopifnot(timeframe %in% c("daily", "weekly"), years > 0)

days_back <- ceiling(years * 365)
horizons <- if (timeframe == "weekly") c(1, 4, 12) else c(5, 10, 20)
symbols <- if (uni_kind == "nse") load_universe()$Symbol else fallback_universe()$Symbol

message("Backtesting ", length(symbols), " symbols, ", timeframe, " bars, ", years, " years")

mkt_data <- fetch_history(MARKET_PROXY, days_back)
if (is.null(mkt_data)) stop("Could not download market proxy ", MARKET_PROXY)
mkt_bt <- backtest_scores(mkt_data, timeframe, horizons, is_market = TRUE)
market_scores <- xts::xts(mkt_bt$score, order.by = mkt_bt$date)

results <- list()
for (sym in setdiff(symbols, MARKET_PROXY)) {
  d <- fetch_history(sym, days_back)
  bt <- if (!is.null(d)) tryCatch(backtest_scores(d, timeframe, horizons, market_scores),
                                  error = function(e) NULL)
  if (is.null(bt)) { message("  skipped ", sym, " (no data)"); next }
  bt$symbol <- sym
  results[[sym]] <- bt
}
if (length(results) == 0) stop("No data downloaded.")
all_bt <- do.call(rbind, results)

cat("\nScore bands vs forward returns (", timeframe, ", ", length(results), " stocks, ",
    nrow(all_bt), " stock-bars)\n", sep = "")
cat("Columns: mean forward return % and hit rate % (share of positive returns)\n\n")
print(summarise_backtest(all_bt), row.names = FALSE)

baseline <- vapply(paste0("fwd_", horizons), function(col) round(mean(all_bt[[col]], na.rm = TRUE) * 100, 2), numeric(1))
cat("\nAll bars (baseline) mean forward return %:\n"); print(baseline)

out <- file.path(tempdir(), paste0("backtest_", timeframe, ".csv"))
write.csv(all_bt, out, row.names = FALSE)
cat("\nPer-bar results written to", out, "\n")
