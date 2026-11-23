# Load the app's modules (not global.R: tests must not touch the network or start workers).
for (f in c("scoring.R", "data.R", "backtest.R", "ai.R")) {
  source(file.path("..", "..", "R", f), local = FALSE, encoding = "UTF-8")
}

# Synthetic daily OHLCV series: `closes` drives the close; volume is constant
# unless `last_volume` overrides the final bar.
make_ohlcv <- function(closes, volume = 1e6, last_volume = volume,
                       start = as.Date("2024-01-01")) {
  n <- length(closes)
  dates <- seq(start, by = "day", length.out = n)
  vol <- rep(volume, n)
  vol[n] <- last_volume
  m <- cbind(Open = closes, High = closes * 1.01, Low = closes * 0.99,
             Close = closes, Volume = vol, Adjusted = closes)
  colnames(m) <- paste0("TEST.", colnames(m))
  xts::xts(m, order.by = dates)
}

# A neutral snapshot: every rule is either inactive or explicitly set by the test.
neutral_snapshot <- function(...) {
  s <- list(price = 100, volume = NA, change = NA, rsi = NA, rsi_prev = NA,
            ema20 = NA, ema50 = NA, ema_slope = NA, macd = NA, macd_sig = NA,
            vol_avg = NA, ret_20 = NA, drawdown = NA)
  utils::modifyList(s, list(...))
}
