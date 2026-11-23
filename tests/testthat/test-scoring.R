test_that("baseline: no trend data scores 50 minus the below-EMA penalty", {
  # Price not above an (unknown) EMA20 counts as below trend: 50 - 10.
  expect_equal(score_snapshot(neutral_snapshot())$score, 40)
})

test_that("trend rules add up", {
  s <- neutral_snapshot(price = 110, ema20 = 100, ema50 = 95, ema_slope = 1)
  # +10 above EMA20, +5 rising slope, +10 EMA20 > EMA50
  expect_equal(score_snapshot(s)$score, 75)
  s$ema_slope <- -1
  expect_equal(score_snapshot(s)$score, 70)
})

test_that("RSI rules", {
  base <- function(...) score_snapshot(neutral_snapshot(price = 110, ema20 = 100, ...))$score
  expect_equal(base(rsi = 60), 70)                 # 60 + 10 sweet spot
  expect_equal(base(rsi = 25), 35)                 # 60 - 25 oversold
  expect_equal(base(rsi = 85), 50)                 # 60 - 10 overbought
  expect_equal(base(rsi = 75), 60)                 # 70-80: neutral
  expect_equal(base(rsi = 60, rsi_prev = 50), 75)  # +5 RSI jump > 5
  # RSI rising while price fell on the bar: +5 divergence
  expect_equal(base(rsi = 60, rsi_prev = 58, change = -1), 75)
  # Missing change must not error
  expect_equal(base(rsi = 60, rsi_prev = 58), 70)
})

test_that("volume ratio counts on daily bars only", {
  s <- neutral_snapshot(price = 110, ema20 = 100, volume = 2e6, vol_avg = 1e6)
  daily <- score_snapshot(s, "daily")
  expect_equal(daily$vol_ratio, 2)
  expect_equal(daily$score, 70)
  weekly <- score_snapshot(s, "weekly")
  expect_equal(weekly$vol_ratio, 1)
  expect_equal(weekly$score, 60)
  expect_equal(score_snapshot(modifyList(s, list(volume = 5e5)))$score, 50)
})

test_that("drawdown deeper than 20% is penalised", {
  s <- neutral_snapshot(price = 110, ema20 = 100)
  expect_equal(score_snapshot(modifyList(s, list(drawdown = -0.10)))$score, 60)
  expect_equal(score_snapshot(modifyList(s, list(drawdown = -0.25)))$score, 45)
})

test_that("MACD band is relative to price, so it is scale-invariant", {
  near <- function(p) score_snapshot(neutral_snapshot(price = p, ema20 = p * 0.9,
                                                      macd = p * 0.0005, macd_sig = 0))$score
  expect_equal(near(50), near(20000))
  expect_equal(near(50), 62)  # 60 + 2 for near-crossover
  above <- score_snapshot(neutral_snapshot(price = 100, ema20 = 90, macd = 1, macd_sig = 0))$score
  below <- score_snapshot(neutral_snapshot(price = 100, ema20 = 90, macd = -1, macd_sig = 0))$score
  expect_equal(c(above, below), c(65, 55))
})

test_that("market context and sector bonus", {
  s <- neutral_snapshot(price = 110, ema20 = 100)
  expect_equal(score_snapshot(s, market_score = 40)$score, 45)
  expect_equal(score_snapshot(s, market_score = 60)$score, 60)
  # The market proxy is never penalised for its own weakness
  expect_equal(score_snapshot(s, market_score = 40, is_market = TRUE)$score, 60)
  expect_equal(score_snapshot(s, in_top_sector = TRUE)$score, 65)
})

test_that("score is clamped to 0-100", {
  low <- neutral_snapshot(rsi = 20, drawdown = -0.5, volume = 1, vol_avg = 10,
                          macd = -5, macd_sig = 0)
  expect_equal(score_snapshot(low, market_score = 10)$score, 0)
  high <- neutral_snapshot(price = 110, ema20 = 100, ema50 = 90, ema_slope = 2, rsi = 65,
                           rsi_prev = 50, change = -1, volume = 3, vol_avg = 1,
                           macd = 5, macd_sig = 0)
  expect_equal(score_snapshot(high, in_top_sector = TRUE)$score, 100)
})

test_that("classify thresholds and weak-market override", {
  expect_equal(classify(80)$verdict, "\u25b2 STRONG BUY")
  expect_equal(classify(75)$verdict, "BUY")
  expect_equal(classify(74)$verdict, "WATCH")
  expect_equal(classify(60)$verdict, "WATCH")
  expect_equal(classify(59)$verdict, "MEDIOCRE")
  expect_equal(classify(41)$verdict, "MEDIOCRE")
  expect_equal(classify(40)$verdict, "\u25bc TRASH (DUMP)")
  expect_equal(classify(70, market_score = 44)$verdict, "NO TRADE ZONE")
  expect_equal(classify(75, market_score = 44)$verdict, "BUY")
  expect_equal(classify(70, market_score = 45)$verdict, "WATCH")
})

test_that("max_drawdown is negative peak-to-trough", {
  expect_equal(max_drawdown(c(100, 120, 90, 110)), 90 / 120 - 1)
  expect_equal(max_drawdown(c(1, 2, 3)), 0)
  expect_true(is.na(max_drawdown(5)))
})

test_that("analyze_data on a steady uptrend gives a bullish result", {
  set.seed(1)
  closes <- 100 * cumprod(1 + 0.004 + rnorm(150, 0, 0.002))
  d <- make_ohlcv(closes, last_volume = 2e6)
  res <- analyze_data(d, "TEST.NS")
  expect_false(is.null(res))
  expect_gte(res$score, 75)
  # The 20-bar average includes the spike bar: 2e6 / ((19 * 1e6 + 2e6) / 20) = 1.9
  expect_equal(res$vol_ratio, 1.9)
  expect_equal(res$price, closes[150])
  expect_true(res$ema20 > res$ema50)
})

test_that("analyze_data on a crash gives a bearish result", {
  closes <- c(seq(100, 120, length.out = 80), seq(120, 70, length.out = 60))
  res <- analyze_data(make_ohlcv(closes, last_volume = 5e5), "TEST.NS")
  expect_lte(res$score, 40)
  expect_equal(res$verdict, "\u25bc TRASH (DUMP)")
  expect_lt(res$drawdown_pct, -20)
})

test_that("analyze_data needs 60 bars, and weekly needs ~2 years of daily data", {
  expect_null(analyze_data(make_ohlcv(rep(100, 59)), "TEST.NS"))
  expect_null(analyze_data(NULL, "TEST.NS"))
  # 200 calendar days of daily bars is far too few weekly bars...
  expect_null(analyze_data(make_ohlcv(100 + 1:200 / 10), "TEST.NS", "weekly"))
  # ...but the 730-day window used for weekly is enough.
  expect_equal(days_back_for("weekly"), 730)
  wk <- analyze_data(make_ohlcv(100 + 1:days_back_for("weekly") / 10), "TEST.NS", "weekly")
  expect_false(is.null(wk))
  expect_equal(wk$vol_ratio, 1)
})

test_that("market proxy gets the market display name and no NO TRADE override", {
  closes <- c(seq(100, 120, length.out = 80), seq(120, 70, length.out = 60))
  res <- analyze_data(make_ohlcv(closes), MARKET_PROXY, market_score = 10, is_market = TRUE)
  expect_equal(res$symbol, "MARKET TREND (NIFTY)")
  expect_false(res$verdict == "NO TRADE ZONE")
})
