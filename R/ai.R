# AI commentary via OpenRouter. Informational only: it never changes the score.
# The key and model are passed in explicitly because these functions run inside
# parallel workers, which do not see environment variables set after startup.

OPENROUTER_URL <- "https://openrouter.ai/api/v1/chat/completions"
DEFAULT_AI_MODEL <- "meta-llama/llama-3-8b-instruct"

# One chat completion. Returns the reply text, or an error string starting with a warning sign.
openrouter_chat <- function(messages, api_key, model, max_tokens = 80,
                            temperature = 0.3, timeout_secs = 20) {
  if (!nzchar(api_key)) return("\u26a0\ufe0f AI skipped: no OPENROUTER_API_KEY configured.")
  resp <- tryCatch(
    httr::POST(
      OPENROUTER_URL,
      httr::add_headers(Authorization = paste("Bearer", api_key)),
      httr::content_type_json(),
      httr::timeout(timeout_secs),
      body = jsonlite::toJSON(list(model = model, messages = messages,
                                   max_tokens = max_tokens, temperature = temperature),
                              auto_unbox = TRUE)
    ),
    error = function(e) e
  )
  if (inherits(resp, "error")) return(paste("\u26a0\ufe0f AI connection failed:", conditionMessage(resp)))

  body <- tryCatch(httr::content(resp, as = "parsed", type = "application/json"),
                   error = function(e) NULL)
  if (httr::status_code(resp) >= 400) {
    msg <- if (!is.null(body$error$message)) body$error$message else httr::http_status(resp)$message
    return(paste0("\u26a0\ufe0f AI error (HTTP ", httr::status_code(resp), "): ", msg))
  }
  text <- tryCatch(body$choices[[1]]$message$content, error = function(e) NULL)
  if (!is.character(text) || !nzchar(trimws(text))) return("\u26a0\ufe0f AI returned an empty reply.")
  trimws(text)
}

# Plain-text summary of an analyze_data() result for the model.
describe_for_ai <- function(t) {
  fmt <- function(x, suffix = "") if (is.null(x) || is.na(x)) "n/a" else paste0(x, suffix)
  paste0(
    "Symbol: ", t$real_ticker, " (", t$timeframe, " bars)\n",
    "Quant score: ", t$score, "/100 -> verdict ", t$verdict, "\n",
    "Price: ", fmt(round(t$price, 2)), " | Last bar change: ", fmt(t$change, "%"),
    " | 20-bar return: ", fmt(t$ret_20, "%"), "\n",
    "EMA20: ", fmt(t$ema20), " | EMA50: ", fmt(t$ema50), " | EMA20 5-bar slope: ", fmt(t$ema_slope_val), "\n",
    "RSI(14): ", fmt(t$rsi), " | MACD: ", fmt(t$macd), " vs signal ", fmt(t$macd_sig), "\n",
    "Volume vs 20-bar avg: ", fmt(t$vol_ratio, "x"), " | Drawdown from recent peak: ", fmt(t$drawdown_pct, "%"), "\n",
    "Market (NIFTY proxy) score: ", fmt(t$market_score)
  )
}

get_ai_double_check <- function(tech, api_key, model = DEFAULT_AI_MODEL) {
  system_prompt <- paste(
    "You are a skeptical trading risk manager double-checking a rules-based signal.",
    "If the verdict is bullish, look for reasons it fails; if bearish, check whether the stock is oversold.",
    "Use only the numbers given. Reply in at most 30 words."
  )
  openrouter_chat(
    list(list(role = "system", content = system_prompt),
         list(role = "user", content = paste0(describe_for_ai(tech), "\nValidate this signal."))),
    api_key, model, max_tokens = 80, temperature = 0.3
  )
}

get_ai_best_pick <- function(tech, api_key, model = DEFAULT_AI_MODEL) {
  openrouter_chat(
    list(list(role = "user", content = paste0(
      describe_for_ai(tech),
      "\nThis is the top-scoring stock in today's scan. In at most 25 words, ",
      "say what in these numbers supports it and the main risk."))),
    api_key, model, max_tokens = 70, temperature = 0.4
  )
}
