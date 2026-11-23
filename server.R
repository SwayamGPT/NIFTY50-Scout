function(input, output, session) {

  current_data  <- reactiveVal(NULL)
  hit_list_data <- reactiveVal(NULL)
  sector_data   <- reactiveVal(NULL)
  market_score  <- reactiveVal(50)
  ai_output_val <- reactiveVal("Pending...")
  ai_busy       <- reactiveVal(FALSE)
  scan_busy     <- reactiveVal(FALSE)

  # Local copies so futures export plain values rather than reaching for globals.
  api_key  <- OPENROUTER_API_KEY
  ai_model <- OPENROUTER_MODEL
  cache    <- stock_cache

  top_sector <- function() {
    sd <- sector_data()
    if (!is.null(sd) && nrow(sd) > 0) sd$Sector[1] else NULL
  }

  process_stock <- function(symbol, tf) {
    withProgress(message = paste("Analyzing", symbol, "..."), {
      lead <- top_sector()
      in_lead <- !is.null(lead) && !is_market_symbol(symbol) &&
        get_sector_name(symbol, sector_lookup) == lead

      res <- get_technical_analysis(symbol, tf, cache, market_score = market_score(),
                                    in_top_sector = in_lead)
      if (is.null(res)) { showNotification("Data Unavailable.", type = "warning"); return(NULL) }

      if (is_market_symbol(symbol)) market_score(res$score)
      current_data(res)

      if (!ai_busy() || is_market_symbol(symbol)) {
        ai_output_val("Checking...")
        ai_busy(TRUE)
        should_run_ai <- res$verdict %in% c("\u25b2 STRONG BUY", "\u25bc TRASH (DUMP)") || is_market_symbol(symbol)

        future_promise({
          if (should_run_ai) get_ai_double_check(res, api_key, ai_model)
          else "AI skipped (low-confidence signal)"
        }) %...>% (function(ai_result) {
          ai_output_val(ai_result)
          ai_busy(FALSE)
        }) %...!% (function(error) {
          # Always release the lock, or every later click reports "AI Busy".
          ai_output_val(paste("\u26a0\ufe0f AI check failed:", conditionMessage(error)))
          ai_busy(FALSE)
        })
      } else {
        ai_output_val("AI Busy (Slow down click)...")
      }
      res
    })
  }

  observe({ isolate(process_stock(MARKET_PROXY, "daily")) }, priority = 10)

  observeEvent(input$run_analysis, {
    req(input$stock_input)
    # The last scan's sector ranking and hit list are kept: the sector ranking
    # feeds the leading-sector bonus and the hit list feeds the CSV export.
    process_stock(input$stock_input, input$timeframe)
  })

  observeEvent(input$scan_market, {
    if (scan_busy()) { showNotification("A scan is already running.", type = "message"); return() }
    scan_busy(TRUE)

    tf_val <- input$timeframe
    mkt <- get_technical_analysis(MARKET_PROXY, tf_val, cache)
    curr_mkt_score <- if (!is.null(mkt)) mkt$score else 50
    market_score(curr_mkt_score)

    hit_list_data(NULL)
    sector_data(NULL)
    ai_output_val("Firing up parallel scanners...")

    # One chunk per worker, each its own future, so chunks run in parallel
    # without blocking the Shiny session.
    universe_syms <- live_scan_list
    chunks <- split(universe_syms, cut(seq_along(universe_syms), min(n_workers, length(universe_syms)), labels = FALSE))
    chunk_promises <- lapply(chunks, function(syms) {
      future_promise(scan_symbols(syms, tf_val, curr_mkt_score, cache), seed = TRUE)
    })

    scan <- promise_all(.list = unname(chunk_promises)) %...>% (function(parts) {
      do.call(rbind, parts[!vapply(parts, is.null, logical(1))])
    }) %...>% (function(cands) {

      if (is.null(cands) || nrow(cands) == 0) {
        hit_list_data(data.frame(Symbol = character(), Sector = character(), Price = numeric(),
                                 Score = numeric(), VolRatio = numeric(), Verdict = character(),
                                 Risk = character()))
        ai_output_val("Scan Complete: No strong setups found.")
        return()
      }

      cands$Sector <- get_sector_name(cands$Symbol, sector_lookup)

      # Sector ranking by median score (unmapped symbols excluded).
      ranked <- cands[cands$Sector != "OTHER", ]
      top_sec <- NULL
      if (nrow(ranked) > 0) {
        sec_df <- aggregate(Score ~ Sector, data = ranked, FUN = median)
        names(sec_df)[2] <- "MedianScore"
        sec_df <- sec_df[order(-sec_df$MedianScore), ]
        sector_data(sec_df)
        top_sec <- sec_df$Sector[1]
        in_top <- cands$Sector == top_sec
        cands$Score[in_top] <- pmin(cands$Score[in_top] + 5, 100)
      }

      # Same classification as single-stock analysis, including the weak-market override.
      cls <- lapply(cands$Score, classify, market_score = curr_mkt_score)
      cands$Verdict <- vapply(cls, `[[`, character(1), "verdict")
      cands$Risk    <- vapply(cls, `[[`, character(1), "risk")
      cands <- cands[, c("Symbol", "Sector", "Price", "Score", "VolRatio", "Verdict", "Risk")]

      final_list <- cands[cands$Score >= 70 & cands$Verdict != "NO TRADE ZONE", ]
      final_list <- final_list[order(-final_list$Score), ]
      hit_list_data(final_list)

      if (nrow(final_list) == 0) {
        ai_output_val("Scan Complete: Market looks weak.")
        return()
      }

      top <- final_list[1, ]
      in_lead <- !is.null(top_sec) && top$Sector == top_sec
      ai_output_val(paste("Finding AI narrative for", top$Symbol, "..."))
      future_promise({
        tech <- get_technical_analysis(top$Symbol, tf_val, cache, curr_mkt_score, in_lead)
        if (is.null(tech)) "AI unavailable (no data)." else get_ai_best_pick(tech, api_key, ai_model)
      }) %...>% (function(ai_res) {
        ai_output_val(paste("\U0001f3c6 TOP PICK:", top$Symbol, "-", ai_res))
      }) %...!% (function(error) {
        ai_output_val(paste("\U0001f3c6 TOP PICK:", top$Symbol, "- (AI narrative failed)"))
      })

    }) %...!% (function(error) {
      ai_output_val(paste("Scanner Error:", conditionMessage(error)))
    })
    promises::finally(scan, function() scan_busy(FALSE))

    NULL
  })

  output$downloadData <- downloadHandler(
    filename = function() paste0("hit_list_", input$timeframe, "_", Sys.Date(), ".csv"),
    content = function(file) { req(hit_list_data()); write.csv(hit_list_data(), file, row.names = FALSE) }
  )

  output$stock_name <- renderText({ req(current_data()); current_data()$symbol })

  output$verdict_badge <- renderUI({
    req(current_data()); d <- current_data()
    color_code <- switch(d$color, success = "#28a745", primary = "#007bff", danger = "#dc3545",
                         dark = "#333", secondary = "#6c757d", "#ffc107")
    h1(d$verdict, style = paste0("font-weight: 900; color: ", color_code, ";"))
  })

  output$risk_badge <- renderUI({
    req(current_data()); d <- current_data()
    span(class = paste0("badge bg-", d$color), style = "font-size: 1.2rem;", d$risk)
  })

  output$ai_commentary <- renderText({ ai_output_val() })
  output$stock_price <- renderText({
    req(current_data()); paste0("\u20b9", format(round(current_data()$price, 2), big.mark = ","))
  })

  output$sector_card <- renderUI({
    req(sector_data()); df <- sector_data()
    if (nrow(df) > 0) {
      card(card_header(paste("\U0001f525 LEADING SECTOR (Median):", df$Sector[1]), class = "bg-info text-white"),
           tableOutput("sector_table"))
    }
  })
  output$sector_table <- renderTable({ req(sector_data()); sector_data() }, width = "100%", striped = TRUE)

  output$hit_list_card <- renderUI({
    req(hit_list_data()); df <- hit_list_data()
    if (nrow(df) == 0) {
      div(class = "alert alert-danger text-center", h4("MARKET WEAK. NO SETUPS SCORING 70+ FOUND."))
    } else {
      card(card_header("HIT LIST (Score \u2265 70, tradeable market)", class = "bg-success text-white"),
           tableOutput("hit_list_table"))
    }
  })
  output$hit_list_table <- renderTable({ req(hit_list_data()); hit_list_data() }, width = "100%", striped = TRUE)

  output$metrics_table <- renderTable({
    req(current_data()); d <- current_data()
    data.frame(Metric = c("RSI (14)", "EMA Slope", "Vol Ratio", "Drawdown %", "20-bar Return %"),
               Value = c(d$rsi, d$ema_slope_val, d$vol_ratio, d$drawdown_pct, d$ret_20))
  }, width = "100%", striped = TRUE)

  output$score_circle <- renderUI({
    req(current_data()); score <- current_data()$score
    color <- if (score >= 80) "#28a745" else if (score <= 40) "#dc3545" else "#ffc107"
    div(style = paste0("width: 100px; height: 100px; border-radius: 50%; border: 8px solid ", color,
                       "; margin: 0 auto; display: flex; align-items: center; justify-content: center;",
                       " font-size: 2rem; font-weight: bold; color: ", color, ";"), score)
  })

  output$trend_chart <- renderPlot({
    req(current_data())
    chartSeries(current_data()$history, theme = chartTheme("white"), type = "line",
                TA = "addEMA(n=20, col='blue'); addEMA(n=50, col='red'); addVo()",
                name = current_data()$symbol)
  })
}
