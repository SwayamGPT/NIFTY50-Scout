page_sidebar(
  theme = bs_theme(bootswatch = "litera"),
  title = "NIFTY50 Scout",

  sidebar = sidebar(
    selectInput("timeframe", "Timeframe Strategy:",
                choices = c("Swing (Daily)" = "daily", "Positional (Weekly)" = "weekly")),

    selectizeInput("stock_input", "Analyze Asset", choices = live_scan_list,
                   options = list(placeholder = "Search Universe...")),

    actionButton("run_analysis", "Judge Stock", class = "btn-dark w-100"),
    hr(),
    h6("Market Scan"),
    actionButton("scan_market", "Scan Market (Parallel)", class = "btn-danger w-100",
                 icon = icon("radiation")),
    br(), br(),
    downloadButton("downloadData", "Export Hit List (CSV)", class = "btn-outline-secondary w-100 btn-sm"),
    div(class = "small text-muted pt-2",
        paste0("Universe: ", length(live_scan_list), " symbols (", attr(universe, "source"), ")"))
  ),

  div(
    class = "container-fluid",

    # 1. VERDICT CARD
    card(
      min_height = "180px",
      card_header("Quantitative Verdict", class = "bg-dark text-white"),
      layout_columns(
        col_widths = breakpoints(xs = 12, md = 6),
        div(class = "p-4 text-center",
            h2(textOutput("stock_name"), style = "margin-bottom: 5px; font-weight: 900;"),
            uiOutput("verdict_badge"),
            h3(textOutput("stock_price"), style = "color: #555;")
        ),
        div(class = "p-4",
            h5("Confidence Level:", class = "text-secondary text-center"),
            div(class = "text-center", uiOutput("risk_badge")),
            hr(),
            div(class = "alert alert-info", style = "margin-top:10px;",
                h6("AI Opinion:", style = "font-weight:bold;"),
                textOutput("ai_commentary"),
                div(class = "text-muted small text-center pt-2", "\u26a0\ufe0f AI commentary is informational only.")
            )
        )
      )
    ),
    br(),

    # 2. SECTOR & HIT LIST
    layout_columns(
      col_widths = breakpoints(xs = 12, md = 12),
      uiOutput("sector_card"),
      uiOutput("hit_list_card")
    ),
    br(),

    # 3. METRICS
    layout_columns(
      col_widths = breakpoints(xs = 12, md = 4),
      card(card_header("Technical DNA"), tableOutput("metrics_table")),
      card(card_header("Scorecard"),
           div(class = "text-center p-4", uiOutput("score_circle"), p("Target: >75", class = "small text-muted"))),
      card(card_header("Trend Chart"), plotOutput("trend_chart", height = "250px"))
    )
  )
)
