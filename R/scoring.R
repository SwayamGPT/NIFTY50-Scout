# Scoring engine: indicators, the 0-100 score, and verdict classification.
# Everything here is pure (no network, no Shiny), so it is covered by tests/testthat.

MIN_BARS <- 60          # bars needed before a symbol can be scored
DRAWDOWN_WINDOW <- 130  # bars used for the max-drawdown check (~6 months daily)

# Calendar days of history needed per timeframe. 200 days gives ~138 daily bars;
# weekly needs ~2 years to get ~100 weekly bars (200 days would give only ~28).
days_back_for <- function(timeframe) if (timeframe == "weekly") 730 else 200

is_valid <- function(x) length(x) == 1 && !is.na(x)

# Max drawdown of a price vector, as a negative fraction (e.g. -0.25 = 25% off the peak).
max_drawdown <- function(prices) {
  prices <- as.numeric(prices)
  prices <- prices[!is.na(prices)]
  if (length(prices) < 2) return(NA_real_)
  min(prices / cummax(prices)) - 1
}

# Indicator series for an OHLCV xts object. One row per bar, so the same function
# serves live analysis (last row) and backtests (every row).
compute_indicators <- function(data) {
  cl  <- quantmod::Cl(data)
  vol <- quantmod::Vo(data)
  rsi   <- TTR::RSI(cl, n = 14)
  ema20 <- TTR::EMA(cl, n = 20)
  ema50 <- TTR::EMA(cl, n = 50)
  macd  <- TTR::MACD(cl, nFast = 12, nSlow = 26, nSig = 9)
  dd <- zoo::rollapplyr(as.numeric(cl), DRAWDOWN_WINDOW, max_drawdown, partial = TRUE)

  out <- cbind(
    cl,
    vol,
    TTR::ROC(cl, type = "discrete") * 100,
    rsi,
    stats::lag(rsi, 5),
    ema20,
    ema50,
    ema20 - stats::lag(ema20, 5),
    macd[, "macd"],
    macd[, "signal"],
    TTR::SMA(vol, n = 20),
    TTR::ROC(cl, n = 20, type = "discrete") * 100,
    xts::xts(dd, order.by = zoo::index(cl))
  )
  colnames(out) <- c("price", "volume", "change", "rsi", "rsi_prev", "ema20", "ema50",
                     "ema_slope", "macd", "macd_sig", "vol_avg", "ret_20", "drawdown")
  out
}

# Indicator values at row i, as a plain named list.
snapshot_at <- function(indicators, i = nrow(indicators)) {
  as.list(stats::setNames(as.numeric(indicators[i, ]), colnames(indicators)))
}

# Score one snapshot. Returns the clamped 0-100 score and the volume ratio used.
#   market_score:  score of the market proxy (NULL = unknown, no market adjustment)
#   in_top_sector: TRUE if the stock belongs to the current leading sector
#   is_market:     TRUE when scoring the market proxy itself
score_snapshot <- function(s, timeframe = "daily", market_score = NULL,
                           in_top_sector = FALSE, is_market = FALSE) {
  score <- 50
  price <- s$price

  # 1. Market context
  if (!is.null(market_score) && !is_market && market_score < 50) score <- score - 15

  # 2. Trend & slope
  if (is_valid(s$ema20) && price > s$ema20) {
    score <- score + 10
    if (is_valid(s$ema_slope) && s$ema_slope > 0) score <- score + 5
  } else {
    score <- score - 10
  }
  if (is_valid(s$ema20) && is_valid(s$ema50) && s$ema20 > s$ema50) score <- score + 10

  # 3. Momentum
  if (is_valid(s$rsi)) {
    if (s$rsi > 50 && s$rsi < 70) score <- score + 10
    if (s$rsi < 30) score <- score - 25
    if (s$rsi > 80) score <- score - 10
    if (is_valid(s$rsi_prev)) {
      if (s$rsi - s$rsi_prev > 5) score <- score + 5
      # Bullish divergence: RSI rising while price fell on the bar
      if (s$rsi > s$rsi_prev && is_valid(s$change) && s$change < 0) score <- score + 5
    }
  }

  # 4. Volume (daily only; weekly volume is too coarse)
  vol_ratio <- 1.0
  if (timeframe == "daily" && is_valid(s$volume) && is_valid(s$vol_avg) && s$vol_avg > 0) {
    vol_ratio <- round(s$volume / s$vol_avg, 2)
    if (vol_ratio > 1.3) score <- score + 10
    if (vol_ratio < 0.7) score <- score - 10
  }

  # 5. Drawdown: penalise names more than 20% off their recent peak
  if (is_valid(s$drawdown) && s$drawdown < -0.20) score <- score - 15

  # 6. MACD. The gap is measured relative to price so the "near crossover"
  # band means the same thing for a Rs 50 stock and a Rs 20,000 stock.
  if (is_valid(s$macd) && is_valid(s$macd_sig) && is_valid(price) && price > 0) {
    if (abs(s$macd - s$macd_sig) / price < 0.001) {
      score <- score + 2
    } else if (s$macd > s$macd_sig) {
      score <- score + 5
    } else {
      score <- score - 5
    }
  }

  # 7. Leading-sector bias
  if (isTRUE(in_top_sector)) score <- score + 5

  list(score = max(min(score, 100), 0), vol_ratio = vol_ratio)
}

# Map a score (and market context) to verdict, Bootstrap colour and risk label.
# Used by both single-stock analysis and the market scan so they always agree.
classify <- function(score, market_score = NULL) {
  out <- if (score >= 80) {
    list(verdict = "\u25b2 STRONG BUY", color = "success", risk = "\U0001f48e HIGH CONVICTION")
  } else if (score >= 75) {
    list(verdict = "BUY", color = "primary", risk = "\U0001f680 MOMENTUM")
  } else if (score >= 60) {
    list(verdict = "WATCH", color = "warning", risk = "MEDIUM RISK")
  } else if (score <= 40) {
    list(verdict = "\u25bc TRASH (DUMP)", color = "danger", risk = "HIGH RISK")
  } else {
    list(verdict = "MEDIOCRE", color = "secondary", risk = "\u26a0\ufe0f SPECULATIVE")
  }
  if (!is.null(market_score) && market_score < 45 && score < 75) {
    out <- list(verdict = "NO TRADE ZONE", color = "dark", risk = "\u26d4 MARKET WEAK")
  }
  out
}

# Full analysis of an OHLCV xts object (daily bars). Returns NULL if there is
# not enough history. Pure: the caller fetches the data.
analyze_data <- function(data, symbol, timeframe = "daily", market_score = NULL,
                         in_top_sector = FALSE, is_market = FALSE) {
  if (is.null(data)) return(NULL)
  if (timeframe == "weekly") data <- xts::to.weekly(data)
  if (nrow(data) < MIN_BARS) return(NULL)

  ind <- compute_indicators(data)
  s <- snapshot_at(ind)
  if (!is_valid(s$price)) return(NULL)

  sc <- score_snapshot(s, timeframe, market_score, in_top_sector, is_market)
  cls <- classify(sc$score, if (is_market) NULL else market_score)

  list(
    symbol = if (is_market) "MARKET TREND (NIFTY)" else symbol,
    real_ticker = symbol,
    timeframe = timeframe,
    price = s$price,
    change = round(s$change, 2),
    rsi = round(s$rsi, 1),
    vol_ratio = sc$vol_ratio,
    score = sc$score,
    verdict = cls$verdict,
    color = cls$color,
    risk = cls$risk,
    ema20 = round(s$ema20, 2),
    ema50 = round(s$ema50, 2),
    ema_slope_val = round(s$ema_slope, 2),
    macd = round(s$macd, 3),
    macd_sig = round(s$macd_sig, 3),
    drawdown_pct = round(s$drawdown * 100, 1),
    ret_20 = round(s$ret_20, 2),
    market_score = market_score,
    history = data
  )
}
