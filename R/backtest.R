# Backtest helpers: score every historical bar and measure what happened next.
# Answers "do higher scores actually lead to better forward returns?".
# Run scripts/backtest.R for a report on real data.

# Score each bar of `data` (daily OHLCV xts) and attach forward returns.
#   market_scores: optional xts of market-proxy scores, aligned by date
#   in_top_sector is not modelled (sector leadership is only known at scan time).
backtest_scores <- function(data, timeframe = "daily", horizons = c(5, 10, 20),
                            market_scores = NULL, is_market = FALSE) {
  if (timeframe == "weekly") data <- xts::to.weekly(data)
  n <- nrow(data)
  if (n < MIN_BARS + 1) return(NULL)

  ind <- compute_indicators(data)
  close <- as.numeric(ind$price)
  dates <- zoo::index(ind)
  # Market score as of each bar date (last known value; handles weekly bars).
  mkt <- NULL
  if (!is.null(market_scores)) {
    aligned <- merge(market_scores, xts::xts(order.by = dates), join = "outer")
    mkt <- as.numeric(zoo::na.locf(aligned, na.rm = FALSE)[dates])
  }

  rows <- MIN_BARS:n
  scores <- vapply(rows, function(i) {
    ms <- if (!is.null(mkt) && !is.na(mkt[i])) mkt[i] else NULL
    score_snapshot(snapshot_at(ind, i), timeframe, ms, FALSE, is_market)$score
  }, numeric(1))

  out <- data.frame(date = dates[rows], score = scores)
  for (h in horizons) {
    fwd <- c(close[(1 + h):n] / close[1:(n - h)] - 1, rep(NA, h))
    out[[paste0("fwd_", h)]] <- fwd[rows]
  }
  out
}

# Average forward return and hit rate (share of positive returns) per score band.
# Bands match classify(): [lower, upper) on integer scores.
summarise_backtest <- function(bt, breaks = c(-Inf, 41, 60, 75, 80, Inf),
                               labels = c("<=40 TRASH", "41-59 MEDIOCRE", "60-74 WATCH",
                                          "75-79 BUY", "80+ STRONG BUY")) {
  bt$band <- cut(bt$score, breaks = breaks, labels = labels, right = FALSE)
  fwd_cols <- grep("^fwd_", names(bt), value = TRUE)
  res <- lapply(split(bt, bt$band), function(d) {
    row <- data.frame(n = nrow(d))
    for (col in fwd_cols) {
      x <- d[[col]][!is.na(d[[col]])]
      row[[paste0(col, "_mean_pct")]] <- if (length(x)) round(mean(x) * 100, 2) else NA
      row[[paste0(col, "_hit_pct")]]  <- if (length(x)) round(mean(x > 0) * 100, 1) else NA
    }
    row
  })
  out <- do.call(rbind, res)
  cbind(band = rownames(out), out, row.names = NULL)
}
