# NIFTY50 Scout

A Shiny app that scores Indian (NSE) stocks from 0 to 100 using trend, momentum,
volume, drawdown and MACD rules. It can judge a single stock or scan the whole
NIFTY 500 in parallel, rank sectors, and build a hit list. An optional LLM
(via OpenRouter) adds a short second opinion.

> **Not investment advice.** The scores come from fixed technical rules. Run the
> backtest (below) before trusting them, and treat the AI commentary as informational only.

## Setup

Requires R 4.3 or newer (the lockfile was recorded with R 4.5.3).

```r
install.packages("renv")   # once
renv::restore()            # installs the exact package versions in renv.lock
```

Opening R in the project folder activates renv automatically (via `.Rprofile`).

### API key (optional)

AI commentary uses [OpenRouter](https://openrouter.ai). Without a key the app
still works and simply skips the AI step. To enable it, create a file named `.env`
in the project root:

```
OPENROUTER_API_KEY=sk-or-...
# Optional: any OpenRouter model id (default: meta-llama/llama-3-8b-instruct)
OPENROUTER_MODEL=meta-llama/llama-3-8b-instruct
# Optional: number of parallel scan workers (default: cores - 1, max 8)
SCOUT_WORKERS=4
```

`.env` is in `.gitignore`. Never commit it.

## Run

```r
shiny::runApp()
```

or from a shell:

```bash
Rscript -e 'shiny::runApp(launch.browser = TRUE)'
```

On startup the app downloads the NIFTY 500 list from NSE. If that fails, it uses a
built-in NIFTY 50 list and logs a message. The sidebar shows which list is in use.
Price data comes from Yahoo Finance and is cached for 15 minutes.

## Tests

```bash
Rscript tests/testthat.R
```

The tests cover the scoring rules, verdict thresholds, indicator maths, cache keys,
NSE list parsing and the backtest helpers. They use synthetic prices and need no network.

## Backtest

The weights in `R/scoring.R` (+10 for price above EMA20, -25 for RSI under 30, and so on)
are hand-picked. The backtest checks whether higher scores were actually followed by
better returns:

```bash
Rscript scripts/backtest.R daily 3            # NIFTY 50, daily bars, 3 years
Rscript scripts/backtest.R weekly 5 nse       # NIFTY 500, weekly bars, 5 years
```

It prints the average forward return and hit rate for each verdict band. If
"80+ STRONG BUY" doesn't beat "41-59 MEDIOCRE", the rules need work. Caveats: it uses
today's index members (survivorship bias), ignores costs, and doesn't model the
leading-sector bonus.

## How the score works

Every stock starts at 50. Rules then add or subtract points:

| Rule | Points |
|---|---|
| Market proxy (NIFTYBEES) scores under 50 | -15 |
| Price above EMA20 / below it | +10 / -10 |
| EMA20 rising over 5 bars (when above it) | +5 |
| EMA20 above EMA50 | +10 |
| RSI 50-70 / under 30 / over 80 | +10 / -25 / -10 |
| RSI up more than 5 points in 5 bars | +5 |
| RSI rising while price fell (divergence) | +5 |
| Volume over 1.3x / under 0.7x its 20-bar average (daily only) | +10 / -10 |
| More than 20% below the recent (130-bar) peak | -15 |
| MACD within 0.1% of price of its signal / above / below | +2 / +5 / -5 |
| Stock is in the leading sector from the last scan | +5 |

The score is clamped to 0-100 and mapped to a verdict: 80+ STRONG BUY, 75-79 BUY,
60-74 WATCH, 41-59 MEDIOCRE, 40 and under TRASH. When the market scores under 45,
anything below 75 becomes NO TRADE ZONE. The scan's hit list shows stocks scoring
70 or more, excluding NO TRADE ZONE.

## Project layout

```
global.R         startup: packages, config, worker pool, stock universe
ui.R, server.R   Shiny app
R/scoring.R      indicators, score rules, verdicts (pure functions)
R/data.R         NSE universe, sector lookup, Yahoo download + cache, scan worker
R/ai.R           OpenRouter calls
R/backtest.R     backtest helpers
scripts/         backtest.R command-line report
tests/testthat/  unit tests
renv.lock        pinned package versions
```

## Maintaining the ticker list

Index membership changes twice a year, and companies get renamed or demerged. The
live NSE list handles this automatically. The built-in fallback in `R/data.R`
(`fallback_sector_map`) needs updating by hand: compare it with NSE's
[NIFTY 50 constituents](https://www.niftyindices.com/indices/equity/broad-based-indices/nifty--50)
and check that each `.NS` ticker still resolves on Yahoo Finance.

The fallback list was last revised in November 2026 from index changes known up to late
2025, but **has not been checked against live NSE data**. Changes from the previous list:

- Removed: LTIM, DIVISLAB (replaced Sep 2024), BPCL, BRITANNIA (Mar 2025),
  HEROMOTOCO, INDUSINDBK (Sep 2025), and TATAMOTORS, which demerged in Oct 2025.
- Added: SBILIFE, SHRIRAMFIN, TRENT, BEL, ETERNAL (formerly Zomato), JIOFIN,
  INDIGO, MAXHEALTH, and TMPV (Tata Motors Passenger Vehicles).
- Any index changes after late 2025 are not reflected.
