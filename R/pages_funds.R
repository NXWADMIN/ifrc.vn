# Dashboard, fund list and the fund page (Overview, Performance, Composition, Resources).

sparkline_svg <- function(v, w = 96, h = 28) {
  v <- tail(v, 63)
  if (length(v) < 2) return(NULL)
  x <- seq(0, w, length.out = length(v)); y <- h - 2 - (v - min(v)) / max(diff(range(v)), 1e-9) * (h - 4)
  col <- if (v[length(v)] >= v[1]) "var(--fms-up)" else "var(--fms-down)"
  HTML(sprintf('<svg class="fms-spark" width="%d" height="%d" viewBox="0 0 %d %d" aria-hidden="true"><polyline fill="none" stroke="%s" stroke-width="1.6" points="%s"/></svg>',
               w, h, w, h, col, paste(sprintf("%.1f,%.1f", x, y), collapse = " ")))
}

# ---------- fund list (module): header sort, filter popovers per column, rows as cards
FUND_COLS <- list(
  fund_name = list(label = "Name", filter = TRUE), country = list(label = "Country", filter = TRUE),
  selection = list(label = "Selection", filter = TRUE), ratio_label = list(label = "Ratio", filter = TRUE),
  currency = list(label = "Cur", filter = TRUE), components = list(label = "Comp.", num = TRUE))
PERF_COLS <- list(ytd = list(label = "YTD %", num = TRUE, pct = TRUE), m12 = list(label = "12M %", num = TRUE, pct = TRUE))
EXT_COLS <- list(nav = list(label = "NAV", num = TRUE), d1 = list(label = "1D %", num = TRUE, pct = TRUE), ytd = PERF_COLS$ytd, m12 = PERF_COLS$m12,
                 aum_usd = list(label = "AUM (USD)", num = TRUE), spark = list(label = "3M trend", nosort = TRUE))

fund_grid_ui <- function(id) div(class = "fms-grid-wrap", uiOutput(NS(id, "head")), uiOutput(NS(id, "body")), rds_source(c("fms_fund_list", "fms_funds_prices", "fms_funds_specs")))

fund_grid_server <- function(id, rows, extended = FALSE) moduleServer(id, function(input, output, session) {
  cols <- c(FUND_COLS, if (extended) EXT_COLS else PERF_COLS)
  ns <- session$ns
  sort <- reactiveValues(key = NULL, dir = 1)
  observeEvent(input$sort, {
    k <- input$sort$key
    sort$dir <- if (identical(sort$key, k)) -sort$dir else if (isTRUE(cols[[k]]$num)) -1 else 1
    sort$key <- k
  })
  # The fund name takes 30% of the width; the figures share the rest.
  # Text columns get room for their label and filter icon (French labels are the longest).
  col_w <- c(country = "minmax(84px, 1fr)", selection = "minmax(112px, 1.2fr)", ratio_label = "minmax(72px, 0.9fr)",
             currency = "minmax(64px, 0.8fr)", components = "minmax(60px, 0.6fr)", spark = "minmax(96px, 1.1fr)")
  widths <- vapply(names(cols)[-1], function(k) if (k %in% names(col_w)) col_w[[k]] else if (extended) "minmax(60px, 1fr)" else "minmax(70px, 1fr)", "")
  grid_style <- sprintf("grid-template-columns: minmax(%s, 30%%) %s", if (extended) "220px" else "240px", paste(widths, collapse = " "))

  output$head <- renderUI({
    f <- rows(); key <- sort$key; dir <- sort$dir
    div(class = "fms-grid-head", style = grid_style, lapply(names(cols), function(k) {
      c <- cols[[k]]
      label <- if (isTRUE(c$nosort)) tags$span(c$label) else
        tags$button(class = paste("fms-th", if (identical(key, k)) "on"), onclick = sprintf("Shiny.setInputValue('%s', {key: '%s'}, {priority: 'event'})", ns("sort"), k),
                    c$label, if (identical(key, k)) icon(if (dir > 0) "arrow-up" else "arrow-down"))
      filt <- if (isTRUE(c$filter)) {
        body <- if (k == "fund_name") textInput(ns("f_fund_name"), NULL, placeholder = "Fund name or code")
                else checkboxGroupInput(ns(paste0("f_", k)), NULL, sort(unique(as.character(f[[k]]))))
        popover(tags$button(class = "fms-th fms-filter", title = paste(tr("Filter"), c$label), icon("filter")), body, placement = "bottom")
      }
      div(class = paste("fms-gh", if (isTRUE(c$num)) "num"), label, filt)
    }))
  })

  output$body <- renderUI({
    f <- rows()
    for (k in names(cols)) {
      v <- input[[paste0("f_", k)]]
      if (k == "fund_name" && nz(v, "") != "") f <- f[grepl(tolower(v), tolower(paste(f$fund_name, f$fund_code)), fixed = TRUE), ]
      else if (k != "fund_name" && length(v)) f <- f[as.character(f[[k]]) %in% v, ]
    }
    if (!is.null(sort$key)) f <- f[order(f[[sort$key]], decreasing = sort$dir < 0, na.last = TRUE), ]
    if (!nrow(f)) return(div(class = "fms-empty", "No fund matches these filters."))
    div(class = "fms-grid-body", lapply(seq_len(nrow(f)), function(i) {
      r <- f[i, ]
      cell <- function(k) {
        c <- cols[[k]]
        switch(k,
          fund_name = div(class = "fms-fund-name", tags$span(class = "fms-fund-icon", icon("building-columns")), div(tags$b(r$fund_name))),
          selection = pill(r$selection, if (r$selection == "COMBINED") "combined" else "active"),
          ratio_label = tags$span(class = "fms-ratio", r$ratio_label),
          nav = fmt_nav(r$nav, r$currency),
          aum_usd = fmt_compact(r$aum_usd),
          spark = sparkline_svg(fund_series(r$fund_id)$v),
          if (isTRUE(c$pct)) tags$b(pct_cell(r[[k]])) else as.character(r[[k]]))
      }
      nav_link(page = "fund", fund = r$fund_code, tab = "overview", class = "fms-grid-row", style = grid_style,
               lapply(names(cols), function(k) div(class = paste("fms-gc", if (isTRUE(cols[[k]]$num)) "num"), cell(k))))
    }))
  })
})

# Best Performers: one card per row of data/rds/fms_top_perf.rds, in order_category order.
# Columns: code, name, date, period, category (the card label), value_pc (in %), order_category.
top_perf <- function() {
  f <- file.path("data", "rds", "fms_top_perf.rds")
  if (!file.exists(f)) return(data.frame())
  d <- as.data.frame(readRDS(f))
  d[order(d$order_category), , drop = FALSE]
}

# ---------- page servers
funds_server <- function(input, output, session, st, go, me, act) {
  my_funds <- reactive({ u <- me(); req(u); fund_rows(u) })
  fund_grid_server("dash_grid", my_funds)
  fund_grid_server("list_grid", my_funds, extended = TRUE)

  output$pg_dashboard <- renderUI({
    u <- me(); req(u); f <- my_funds(); last <- latest_date()
    tp <- top_perf()
    tagList(
      page_head("Dashboard"),
      div(class = "fms-section-head", tags$h2("Best Performers"),
          tags$small(paste(tr("Last update:"), fmt_date(if (nrow(tp)) as.character(max(tp$date)) else last)))),
      div(class = "fms-tiles", lapply(seq_len(nrow(tp)), function(i) {
        b <- tp[i, ]; v <- b$value_pc / 100
        # The table has its own fund codes, so the card links to the fund page by matching the fund name.
        code <- f$fund_code[match(toupper(b$name), toupper(f$fund_name))]
        card <- tagList(
          div(class = "fms-perf-top", tags$span(class = paste0("fms-perf-icon g", (i - 1) %% 4 + 1), icon(if (v >= 0) "arrow-trend-up" else "arrow-trend-down")),
              tags$span(class = paste("fms-perf-badge", if (v < 0) "neg"), icon(if (v >= 0) "arrow-up" else "arrow-down"), fmt_pct(v))),
          div(class = "fms-perf-label", as.character(b$category)), div(class = "fms-perf-name", toupper(b$name)))
        if (!is.na(code)) nav_link(page = "fund", fund = code, tab = "performance", class = "fms-perf-card", card) else div(class = "fms-perf-card", card)
      })),
      rds_source("fms_top_perf"),
      div(class = "fms-section-head", div(tags$h2("Fund List"), tags$small(sprintf(tr("%d funds · performance in fund currency to %s"), nrow(f), fmt_date(last)))),
          nav_link(page = "funds", class = "btn btn-sm btn-outline-secondary", "Open full fund monitor")),
      fund_grid_ui("dash_grid"))
  })

  output$pg_funds <- renderUI({
    req(me())
    tagList(page_head("Funds", sprintf(tr("Every fund you can access, with NAV, performance and assets as of %s. Use the filter icons in the headers to narrow the list."), fmt_date(latest_date()))),
            fund_grid_ui("list_grid"))
  })

  # ---------- fund page
  fund <- reactive({
    u <- me(); req(u, st$fund)
    tryCatch(fund_by_code(st$fund, u), fms_error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
  })

  output$pg_fund <- renderUI({
    f <- fund(); req(f); m <- fund_metrics(f$fund_id)
    tagList(
      nav_link(page = "funds", class = "fms-back", icon("chevron-left"), " All funds"),
      div(class = "fms-fund-hero",
        div(class = "fms-hero-main", div(class = "fms-fund-name big", tags$span(class = "fms-fund-icon", icon("building-columns")), tags$h1(f$fund_name)),
            div(class = "fms-meta", pill(f$fund_code), pill(f$country), pill(f$selection, if (f$selection == "COMBINED") "combined" else "active"),
                tags$span(class = "fms-ratio", f$ratio_label), pill(f$currency), pill(f$share_class))),
        div(class = "fms-nav-box", div(class = "muted", "NAV per unit · ", fmt_date(m$nav_date)),
            div(class = "v", fmt_nav(m$nav, f$currency), tags$small(f$currency)),
            div(class = cls(m$d1), fmt_pct(m$d1), " on the day · YTD ", fmt_pct(m$ytd)))),
      navset_underline(id = "fund_tab", selected = isolate(st$tab),
        nav_panel("Overview", value = "overview", uiOutput("fund_overview")),
        nav_panel("Performance", value = "performance", uiOutput("fund_performance")),
        nav_panel("Composition", value = "composition", uiOutput("fund_composition")),
        nav_panel("Resources", value = "resources", uiOutput("fund_resources"))))
  })
  observeEvent(input$fund_tab, st$tab <- input$fund_tab)

  kv <- function(...) { x <- list(...); tags$dl(class = "fms-kv", lapply(names(x), function(k) tagList(tags$dt(k), tags$dd(x[[k]])))) }

  output$fund_overview <- renderUI({
    f <- fund(); req(f); m <- fund_metrics(f$fund_id)
    s <- q1("SELECT * FROM fms_funds_specs WHERE fund_id = ?", f$fund_id)
    bench <- q1("SELECT benchmark_name FROM fms_benchmark WHERE benchmark_code = ? LIMIT 1", f$benchmark_code)$benchmark_name
    mgr <- q1("SELECT display_name FROM fms_users WHERE user_id = ?", f$manager_user_id)$display_name
    comps <- q1("SELECT COUNT(*) AS n FROM fms_fund_positions WHERE fund_id = ? AND instrument_type NOT IN ('cash','tbill')", f$fund_id)$n
    intra <- q("SELECT ts, inav FROM fms_funds_intraday WHERE fund_id = ? ORDER BY ts", f$fund_id)
    last <- if (nrow(intra)) intra$inav[nrow(intra)] else NA
    chg <- last / m$nav - 1
    money0 <- function(x) paste(fmt_num(x, 0), f$currency)
    tagList(
      layout_columns(col_widths = c(8, 4),
        div(class = "fms-stack",
          fms_card(tagList("Intraday indicative NAV", tags$small(paste(fmt_date(intra$ts[1]), tr("· times in ICT (UTC+7)")))),
               div(class = "fms-row", tags$b(class = "fms-big num", fmt_nav(last, f$currency)), tags$b(class = cls(chg), fmt_pct(chg)),
                   tags$span(class = "muted", sprintf(tr("vs. last NAV %s (%s)"), fmt_nav(m$nav, f$currency), fmt_date(m$nav_date)))),
               plotly::plotlyOutput("intraday_chart", height = "250px")),
          fms_card("Objective", tags$p(class = "fms-prose", f$objective), tags$h3("Strategy"), tags$p(class = "fms-prose", f$strategy))),
        div(class = "fms-stack",
          fms_card("Key facts", kv(`Assets under management` = fmt_compact(m$aum, paste0(f$currency, " ")), `Year to date` = pct_cell(m$ytd),
                               `Rolling 12 months` = pct_cell(m$m12), `Volatility, 1 year` = fmt_pct(m$vol_1y, 1, FALSE), Inception = fmt_date(f$inception_date),
                               Benchmark = bench, Components = comps, `Fund manager` = mgr)),
          fms_card(tagList("Risk indicator", tags$small("SRI 1 (lower) to 7 (higher)")),
               div(class = "fms-sri", lapply(1:7, function(i) tags$span(class = if (i == f$risk_level) "on", i)))))),
      fms_card("Fund specifications", layout_columns(col_widths = c(4, 4, 4),
        kv(ISIN = s$isin, `Legal structure` = s$legal_structure, Domicile = s$domicile, `Management company` = s$management_company,
           Custodian = s$custodian, `Clearing broker` = s$clearing_broker, Auditor = s$auditor, Administrator = s$administrator),
        kv(Dealing = s$dealing_frequency, `Cut-off` = s$cut_off_time, Settlement = s$settlement_cycle, `Minimum initial` = money0(s$min_initial_investment),
           `Minimum additional` = money0(s$min_subsequent), Distribution = s$distribution_policy, Rebalancing = s$rebalancing),
        kv(`Subscription fee` = paste0(tr("up to "), s$subscription_fee_pct, "%"), `Redemption fee` = paste0(s$redemption_fee_pct, "%"),
           `Management fee` = paste0(s$management_fee_pct, tr("% a year")), `Performance fee` = sprintf(tr("%s%% · high-water mark %s"), s$performance_fee_pct, tr(if (s$high_water_mark) "Yes" else "No")),
           `Ongoing charges` = paste0(s$ongoing_charges_pct, "%"), `Max gross exposure` = paste0(s$max_gross_exposure_pct, "%"),
           `Max single position` = paste0(s$max_single_market_pct, "%"), `Min liquidity` = paste0(s$min_liquidity_pct, "%"),
           `Target volatility` = paste0(s$target_volatility_pct, "%")))))
  })

  output$intraday_chart <- plotly::renderPlotly({
    f <- fund(); req(f); m <- fund_metrics(f$fund_id)
    d <- q("SELECT ts, inav FROM fms_funds_intraday WHERE fund_id = ? ORDER BY ts", f$fund_id)
    d$t <- as.POSIXct(d$ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    attr(d$t, "tzone") <- "Asia/Ho_Chi_Minh"
    plotly::plot_ly(d, x = ~t, y = ~inav, type = "scatter", mode = "lines", name = "iNAV", line = list(color = SERIES[1], width = 2),
                    hovertemplate = "%{y:,.4f}<extra>iNAV</extra>") |>
      plotly::add_lines(y = m$nav, name = tr("Last NAV"), line = list(color = "rgba(140,160,190,0.6)", dash = "dot", width = 1), hoverinfo = "skip") |>
      fms_plot_layout(legend = FALSE) |> plotly::layout(xaxis = list(tickformat = "%H:%M"))
  })

  # Performance tab
  # The comparison starts as the VN-Index (Vietnam funds) or the S&P 500 (global funds); the list lets the user pick any index, commodity or crypto.
  bench_pick <- reactiveVal(NULL)
  observeEvent(fund(), bench_pick(NULL))
  observeEvent(input$perf_bench, if (!identical(input$perf_bench, bench_pick())) bench_pick(input$perf_bench), ignoreInit = TRUE)
  perf <- reactive({
    f <- fund(); req(f); s <- fund_series(f$fund_id)
    code <- nz(bench_pick(), default_comparison(f))
    b <- comparison_series(code, s$d[1])
    list(f = f, s = s, b = b, code = code, m = fund_metrics(f$fund_id), bm = if (nrow(b) > 2) metrics(b) else NULL, bench = comparison_name(code))
  })
  output$fund_performance <- renderUI({
    p <- perf(); m <- p$m; bm <- p$bm
    rows <- data.frame(label = c("1 month", "3 months", "6 months", "Year to date", "Rolling 12 months", "Rolling 24 months, annualised",
                                 "Rolling 36 months, annualised", paste(tr("Since"), fmt_date(p$s$d[1]))),
                       key = c("m1", "m3", "m6", "ytd", "m12", "m24a", "m36a", "since_start"))
    tagList(
      fms_card(tagList(p$f$fund_code, " vs. ", p$bench, tags$small("rebased to 100")),
           actions = div(class = "fms-perf-actions",
             selectInput("perf_bench", NULL, benchmark_choices(), selected = p$code, width = "300px"),
             radioButtons("perf_period", NULL, c("1M", "3M", "6M", "YTD", "1Y", "3Y", "Max"), selected = nz(isolate(input$perf_period), "1Y"), inline = TRUE)),
           plotly::plotlyOutput("perf_chart", height = "330px")),
      layout_columns(col_widths = c(6, 6),
        fms_card(tagList("Returns", tags$small(sprintf(tr("in %s"), p$f$currency))),
             html_table(source = c("fms_funds_prices", "fms_benchmark", "fms_market_prices"), rows, list(Period = function(r) r$label, `#Fund` = function(r) tags$b(pct_cell(m[[r$key]])),
                                   `#Benchmark` = function(r) pct_cell(bm[[r$key]]),
                                   `#Diff.` = function(r) pct_cell(m[[r$key]] - bm[[r$key]])))),
        fms_card(tagList("Calendar-year returns", tags$small("* partial year")), plotly::plotlyOutput("cal_chart", height = "230px"),
             kv(`Volatility, 1 year` = fmt_pct(m$vol_1y, 1, FALSE), `Sharpe ratio, 1 year (4% cash)` = fmt_num(m$sharpe_1y, 2),
                `Maximum drawdown` = tags$span(class = "down", fmt_pct(m$max_drawdown, 1))))))
  })
  output$perf_chart <- plotly::renderPlotly({
    p <- perf(); per <- nz(input$perf_period, "1Y"); last <- p$s$d[nrow(p$s)]
    from <- switch(per, `1M` = add_months(last, -1), `3M` = add_months(last, -3), `6M` = add_months(last, -6),
                   YTD = sprintf("%d-12-31", as.integer(substr(last, 1, 4)) - 1), `1Y` = add_months(last, -12), `3Y` = add_months(last, -36), p$s$d[1])
    if (nrow(p$b)) from <- max(from, p$b$d[1])  # market histories can start later than the fund: rebase both from the same day
    s <- p$s[p$s$d >= from, ]; b <- p$b[p$b$d >= from, ]
    plotly::plot_ly() |>
      plotly::add_lines(x = s$date, y = s$v / s$v[1] * 100, name = p$f$fund_code, line = list(color = SERIES[1], width = 2), hovertemplate = "%{y:.2f}") |>
      plotly::add_lines(x = b$date, y = b$v / b$v[1] * 100, name = p$bench, line = list(color = SERIES[2], width = 2), hovertemplate = "%{y:.2f}") |>
      fms_plot_layout()
  })
  output$cal_chart <- plotly::renderPlotly({
    p <- perf(); c <- calendar_returns(p$s, p$b)
    lab <- paste0(c$year, ifelse(c$partial, "*", ""))
    plotly::plot_ly(x = lab, y = c$fund, type = "bar", name = tr("Fund"), marker = list(color = SERIES[1]), hovertemplate = "%{y:.2%}") |>
      plotly::add_bars(y = c$benchmark, name = tr("Benchmark"), marker = list(color = SERIES[2]), hovertemplate = "%{y:.2%}") |>
      fms_plot_layout(yfmt = ".0%") |> plotly::layout(barmode = "group", bargap = 0.35)
  })

  # Composition tab
  output$fund_composition <- renderUI({
    f <- fund(); req(f); st$tick; c <- composition(f); p <- c$positions
    futures <- any(p$instrument_type == "future")
    tiles <- if (futures) list(stat_tile("Components", paste(sum(p$instrument_type == "future"), "futures")), stat_tile("Gross exposure", fmt_pct(c$gross_exposure, 1, FALSE)),
                               stat_tile("Net exposure", fmt_pct(c$net_exposure, 1)), stat_tile("Initial margin used", paste(fmt_compact(c$margin_used, paste0(f$currency, " ")), "·", fmt_pct(c$margin_used / c$nav, 1, FALSE))))
             else list(stat_tile("Components", paste(sum(p$instrument_type != "cash"), "holdings")), stat_tile("Invested", fmt_pct(c$gross_exposure, 1, FALSE)),
                       stat_tile("Growth / protective", paste(fmt_pct(c$by_sleeve$net[c$by_sleeve$name != "Liquidity"], 0, FALSE), collapse = " / ")),
                       stat_tile("Cash and deposits", fmt_pct(c$liquidity, 1, FALSE)))
    # Quantity as a number, and its unit in the next column.
    qty <- function(r) switch(r$instrument_type, future = paste0(if (r$contracts > 0) "+" else "", fmt_num(r$contracts, 0)),
                              equity = , bond = fmt_num(r$contracts, 0), tbill = fmt_compact(r$contracts * 100), deposit = fmt_compact(r$contracts), "\u2013")
    unit <- function(r) switch(r$instrument_type, future = "contracts", equity = "shares", bond = "bonds", tbill = "face value", deposit = r$price_currency, "")
    p <- p[order(-abs(p$weight)), ]
    tagList(
      div(class = "fms-tiles", tiles),
      layout_columns(col_widths = c(6, 6),
        fms_card(tagList(if (futures) "Collateral and exposure by sleeve" else "Allocation by sleeve", tags$small(paste(tr("% of NAV"), fmt_compact(c$nav, paste0(f$currency, " "))))),
             html_table(source = "fms_fund_positions", c$by_sleeve, list(Sleeve = function(r) r$name, `#Weight` = function(r)
               if (futures && r$name == "Dynamic futures") sprintf(tr("gross %s · net %s"), fmt_pct(r$gross, 1, FALSE), fmt_pct(r$net, 1)) else fmt_pct(r$net, 1, FALSE))),
             if (futures) tags$p(class = "fms-notice", "Futures exposure is notional; the fund's assets sit in Treasury bills and cash, part of it posted as margin.")),
        fms_card(tagList(if (futures) "Net exposure by market" else "Weight by sector", tags$small(if (futures) "long right, short left" else "% of NAV")),
             plotly::plotlyOutput("sector_chart", height = "320px"))),
      fms_card(tagList("Positions", tags$small(sprintf(tr("%d lines, prices as of the last valuation"), nrow(p)))),
           html_table(source = "fms_fund_positions", class = "fms-table fms-pos-table", p, list(Instrument = function(r) tags$b(r$contract), Description = function(r) r$description, Sector = function(r) r$sector,
                              Sleeve = function(r) r$sleeve, Venue = function(r) r$exchange,
                              `#Quantity` = function(r) tags$span(class = if (r$contracts < 0) "down", qty(r)),
                              Unit = function(r) tags$span(class = "muted", unit(r)),
                              `#Price` = function(r) if (r$instrument_type == "cash") "–" else paste(fmt_num(r$price, if (r$price_currency == "VND") 0 else if (r$price < 10) 4 else 2), r$price_currency),
                              `#Exposure` = function(r) fmt_num(r$exposure, 0),
                              `#% NAV` = function(r) tags$b(class = if (r$weight < 0) "down", fmt_pct(r$weight, 2, FALSE))))))
  })
  output$sector_chart <- plotly::renderPlotly({
    f <- fund(); req(f); st$tick; c <- composition(f)
    futures <- any(c$positions$instrument_type == "future")
    s <- if (futures) {
      fut <- c$positions[c$positions$instrument_type == "future", ]
      data.frame(name = fut$description, net = fut$weight)
    } else c$by_sector[c$by_sector$name != "Cash", ]
    s <- s[order(s$net), ]
    plotly::plot_ly(y = factor(s$name, levels = s$name), x = s$net, type = "bar", orientation = "h",
                    marker = list(color = ifelse(s$net >= 0, SERIES[1], "#f2566b")), hovertemplate = "%{y}: %{x:.1%}<extra></extra>") |>
      fms_plot_layout(legend = FALSE, hovermode = "closest") |> plotly::layout(xaxis = list(tickformat = ".0%"), margin = list(l = 140), bargap = 0.35)
  })

  # Resources tab
  output$fund_resources <- renderUI({
    f <- fund(); req(f)
    d <- q("SELECT * FROM fms_fund_documents WHERE fund_id = ? ORDER BY published_date DESC", f$fund_id)
    fms_card(tagList("Documents", tags$small(sprintf(tr("%d published for %s"), nrow(d), f$fund_code))),
         html_table(source = "fms_fund_documents", d, list(Type = function(r) pill(r$doc_type), Title = function(r) tags$b(r$title), Published = function(r) fmt_date(r$published_date),
                            `#Pages` = function(r) r$pages, Language = function(r) r$language,
                            ` ` = function(r) tags$button(class = "btn btn-sm btn-outline-secondary", `data-action` = "doc", `data-id` = r$document_id, icon("file-lines"), "View"))),
         tags$p(class = "fms-notice", "Document previews are generated from the fund's data in this demo."))
  })

  observeEvent(input$act, guard({
    if (input$act$action != "doc") return()
    u <- me(); doc <- q1("SELECT d.*, f.fund_code FROM fms_fund_documents d JOIN fms_fund_list f USING (fund_id) WHERE document_id = ?", as.integer(input$act$id))
    req(u, doc)
    f <- fund_by_code(doc$fund_code, u); s <- q1("SELECT * FROM fms_funds_specs WHERE fund_id = ?", f$fund_id)
    ser <- fund_series(f$fund_id); ser <- ser[ser$d <= doc$published_date, ]
    m <- if (nrow(ser) > 30) metrics(ser) else NULL
    top <- composition(f)$positions; top <- top[top$instrument_type != "cash", ]; top <- head(top[order(-abs(top$weight)), ], 5)
    showModal(modalDialog(title = doc$title, size = "l", easyClose = TRUE, footer = modalButton("Close"),
      div(class = "fms-doc", div(class = "muted", sprintf(tr("%s · %s · %d pages"), doc$doc_type, fmt_date(doc$published_date), doc$pages)),
          div(class = "muted", s$management_company), tags$h4(class = "fms-cyan", toupper(f$fund_name)), tags$p(class = "fms-prose", f$objective),
          if (!is.null(m)) div(class = "fms-tiles", stat_tile("NAV", paste(fmt_nav(m$nav, f$currency), f$currency)), stat_tile("YTD", fmt_pct(m$ytd)),
                               stat_tile("12 months", fmt_pct(m$m12)), stat_tile("Volatility", fmt_pct(m$vol_1y, 1, FALSE))),
          if (!is.null(m)) tags$p(class = "fms-notice", paste(tr("Figures as of"), fmt_date(m$nav_date))),
          tags$h4("Largest positions"),
          html_table(source = "fms_fund_positions", top, list(Position = function(r) tagList(tags$b(r$contract), " ", r$description), `#Weight` = function(r) fmt_pct(r$weight, 1, FALSE))),
          kv(ISIN = s$isin, `Ongoing charges` = paste0(s$ongoing_charges_pct, "%"), Dealing = paste0(s$dealing_frequency, tr(", cut-off "), s$cut_off_time),
             `Risk indicator` = paste(f$risk_level, tr("of 7"))))))
  }, "document preview"))
}
