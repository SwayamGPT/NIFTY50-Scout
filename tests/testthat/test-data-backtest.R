test_that("cache keys are valid cachem keys and distinct per window", {
  for (sym in c("M&M.NS", "BAJAJ-AUTO.NS", "^NSEI", "TCS.NS")) {
    expect_match(cache_key(sym, 200), "^[a-z0-9_-]+$")
  }
  expect_false(cache_key("TCS.NS", 200) == cache_key("TCS.NS", 730))
})

test_that("cache returns stored data without refetching", {
  cache <- new_stock_cache(tempfile("cache"))
  d <- make_ohlcv(rep(100, 70))
  cache$set(cache_key("FAKE.NS", 200), list(time = Sys.time(), data = d))
  expect_equal(get_cached_stock_data("FAKE.NS", 200, cache), d)
})

test_that("sector lookup falls back to OTHER", {
  lookup <- make_sector_lookup(data.frame(Symbol = c("A.NS", "B.NS"), Sector = c("IT", "AUTO")))
  expect_equal(get_sector_name(c("B.NS", "Z.NS"), lookup), c("AUTO", "OTHER"))
})

test_that("fallback universe is NIFTY 50 sized with unique symbols", {
  uni <- fallback_universe()
  expect_equal(nrow(uni), 50)
  expect_equal(anyDuplicated(uni$Symbol), 0)
  expect_true(all(grepl("\\.NS$", uni$Symbol)))
})

test_that("NSE CSV is parsed into Symbol/Sector", {
  csv <- "Company Name,Industry,Symbol,Series,ISIN Code\nTata Consultancy,Information Technology,TCS,EQ,INE467B01029\nMahindra,Automobile and Auto Components,M&M,EQ,INE101A01026\n"
  uni <- parse_nse_universe(csv)
  expect_equal(uni$Symbol, c("TCS.NS", "M&M.NS"))
  expect_equal(uni$Sector, c("INFORMATION TECHNOLOGY", "AUTOMOBILE AND AUTO COMPONENTS"))
  expect_error(parse_nse_universe("a,b\n1,2\n"))
})

test_that("backtest forward returns line up with prices", {
  closes <- 100 * 1.01^(0:99)
  bt <- backtest_scores(make_ohlcv(closes), horizons = c(1, 5))
  expect_equal(nrow(bt), 100 - MIN_BARS + 1)
  expect_equal(bt$fwd_1[1], 0.01, tolerance = 1e-9)
  expect_equal(bt$fwd_5[1], 1.01^5 - 1, tolerance = 1e-9)
  expect_true(is.na(tail(bt$fwd_5, 1)))
  # Last row's score equals the live analysis on the same data
  expect_equal(tail(bt$score, 1), analyze_data(make_ohlcv(closes), "T.NS")$score)
})

test_that("backtest applies market scores by date", {
  closes <- 100 * 1.01^(0:99)
  d <- make_ohlcv(closes)
  weak <- xts::xts(rep(10, 100), order.by = zoo::index(d))
  bt_plain <- backtest_scores(d, horizons = 1)
  bt_weak  <- backtest_scores(d, horizons = 1, market_scores = weak)
  expect_equal(bt_weak$score, pmax(bt_plain$score - 15, 0))
})

test_that("summary bands match classify() thresholds", {
  bt <- data.frame(score = c(40, 41, 59, 60, 74, 75, 79, 80, 100),
                   fwd_5 = c(-0.1, 0, 0, 0.01, 0.01, 0.02, 0.02, 0.03, 0.05))
  s <- summarise_backtest(bt)
  expect_equal(s$n, c(1, 2, 2, 2, 2))
  expect_equal(s$fwd_5_hit_pct, c(0, 0, 100, 100, 100))
})

test_that("AI without a key is skipped, not attempted", {
  expect_match(openrouter_chat(list(), api_key = "", model = "x"), "no OPENROUTER_API_KEY")
})
