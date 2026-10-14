# Landing pages: one dashboard per fund, laid out like the main Dashboard and built from the fund's own data.
# Menu: Landing Page > AllStar Vietnam Fund, AllStar Commodity Fund.

landing_server <- function(input, output, session, st, go, me, act) {
  # Title in the app-title style (white word, gold serif italic words) with the latest NAV on the right, four Performance cards
  # (YTD, rolling 12 months, rolling 24 / 36 months annualised; each opens the fund's Performance tab),
  # then Overview / Composition / Performance / Documents tabs.
  fund_dashboard <- function(code, plain, gold, pid, bench0) {
    f <- q1("SELECT * FROM fms_fund_list WHERE fund_code = ?", code); req(f)
    m <- fund_metrics(f$fund_id); s <- q1("SELECT * FROM fms_funds_specs WHERE fund_id = ?", f$fund_id)
    perf <- list(c("ytd", "Year To Date %"), c("m12", "Rolling 12 Months %"), c("m24a", "Rolling 24 Months Annualised %"), c("m36a", "Rolling 36 Months Annualised %"))
    tagList(
      div(class = "fms-page-head", div(class = "fms-app-title", tags$span(plain), tags$em(gold), tags$span(class = "fms-pill fms-lp-symbol", f$fund_code)), nav_box(f, m)),
      div(class = "fms-section-head", tags$h2("Performance"), tags$small(paste(tr("Last update:"), fmt_date(m$nav_date)))),
      div(class = "fms-tiles fms-lp-tiles", lapply(seq_along(perf), function(i) {
        v <- m[[perf[[i]][1]]]
        nav_link(page = "fund", fund = f$fund_code, tab = "performance", class = "fms-perf-card",
          div(class = "fms-perf-top", tags$span(class = paste0("fms-perf-icon g", i), icon(if (isTRUE(v < 0)) "arrow-trend-down" else "arrow-trend-up")),
              tags$span(class = paste("fms-perf-badge", if (isTRUE(v < 0)) "neg"), icon(if (isTRUE(v < 0)) "arrow-down" else "arrow-up"), fmt_pct(v))),
          div(class = "fms-perf-label", perf[[i]][2]))
      })),
      rds_source("fms_funds_prices"),
      # Tabs as on the NXW index pages: Overview (specifications + NAV chart by period), Composition, Performance.
      div(class = "fms-lp-tabs", navset_underline(id = paste0(pid, "_tab"),
        nav_panel("Overview", value = "overview",
          # Optional comparison on the NAV chart: none by default, or any benchmark or market index.
          div(class = "fms-lp-bench", selectInput(paste0(pid, "_cmp"), "Compare vs.", c(`No benchmark` = "none", benchmark_choices()), selected = "none")),
          layout_columns(col_widths = c(6, 6), class = "fms-lp-row",
            div(class = "fms-card",
              html_table(class = "fms-table fms-lp-spec", spec_rows(f, m, s), list(Name = function(r) r$name, Information = function(r) r$info)),
              rds_source("fms_fund_list", "fms_funds_specs", "fms_funds_prices")),
            div(class = "fms-card",
              radioButtons(paste0(pid, "_per"), NULL, LP_PERIODS, selected = "1Y", inline = TRUE),
              uiOutput(paste0(pid, "_stats")),
              plotly::plotlyOutput(paste0(pid, "_nav"), height = "330px"), rds_source("fms_funds_prices")))),
        nav_panel("Composition", value = "composition", uiOutput(paste0(pid, "_comp"))),
        nav_panel("Performance", value = "performance",
          # Benchmark selection: the benchmark indices, then market indices, commodities and crypto.
          div(class = "fms-lp-bench", selectInput(paste0(pid, "_bench"), "Benchmark", benchmark_choices(), selected = bench0)),
          uiOutput(paste0(pid, "_perf"))),
        nav_panel("Documents", value = "documents", uiOutput(paste0(pid, "_docs"))))),
      NULL)
  }

  # Latest NAV, its change on the previous NAV and its date, on the right of the title.
  nav_box <- function(f, m) {
    v <- tail(fund_series(f$fund_id)$v, 2); dp <- if (f$currency == "VND") 0 else 2
    d <- if (length(v) == 2) v[2] - v[1] else NA; chg <- if (length(v) == 2) v[2] / v[1] - 1 else NA
    div(class = "fms-lp-nav",
      div(class = "fms-lp-nav-value", fmt_num(m$nav, dp)),
      div(class = paste("fms-lp-nav-chg", if (isTRUE(chg < 0)) "neg" else "pos"),
          paste0(if (isTRUE(d >= 0)) "+", fmt_num(d, dp), " (", fmt_pct(chg), ")")),
      div(class = "fms-lp-nav-date", paste(format(as.Date(m$nav_date), "%Y-%m-%d"), "\u00b7", f$currency)))
  }

  spec_rows <- function(f, m, s) {
    x <- c(`Code` = f$fund_code, `ISIN` = s$isin, `Family` = f$strategy_family, `Currency` = f$currency, `Inception` = fmt_date(f$inception_date),
           `NAV per unit` = paste(fmt_nav(m$nav, f$currency), f$currency), `Assets under management` = fmt_compact(m$aum, paste0(f$currency, " ")),
           `Components` = as.character(f$top_n), `Selection` = paste(f$selection, f$ratio_label), `Rebalancing` = s$rebalancing,
           `Legal structure` = s$legal_structure, `Domicile` = s$domicile, `Dealing` = paste(s$dealing_frequency, "·", s$cut_off_time),
           `Minimum initial` = paste(fmt_num(s$min_initial_investment, 0), f$currency),
           `Fees` = sprintf(tr("Management %s%% a year · performance %s%%"), s$management_fee_pct, s$performance_fee_pct),
           `Distribution` = s$distribution_policy, `Risk indicator` = sprintf(tr("%s of 7"), f$risk_level))
    data.frame(name = tr(names(x)), info = unname(x))
  }

  # One landing page: page output, NAV chart and stats for the chosen period, composition and performance tabs.
  landing_page <- function(pid, code, plain, gold, bench0) {
    fund <- function() { f <- q1("SELECT * FROM fms_fund_list WHERE fund_code = ?", code); req(f); f }
    output[[paste0("pg_lp_", pid)]] <- renderUI({ req(me()); fund_dashboard(code, plain, gold, pid, bench0) })
    bench <- reactive(nz(input[[paste0(pid, "_bench")]], bench0))
    window <- reactive({
      s <- fund_series(fund()$fund_id); last <- s$d[nrow(s)]
      from <- switch(nz(input[[paste0(pid, "_per")]], "1Y"), `1M` = add_months(last, -1), `3M` = add_months(last, -3), `6M` = add_months(last, -6),
                     YTD = sprintf("%d-12-31", as.integer(substr(last, 1, 4)) - 1), `1Y` = add_months(last, -12), `3Y` = add_months(last, -36),
                     `5Y` = add_months(last, -60), s$d[1])
      s[s$d >= from, ]
    })
    output[[paste0(pid, "_stats")]] <- renderUI({
      w <- window(); f <- fund(); r <- w$v[nrow(w)] / w$v[1] - 1
      vol <- sd(diff(log(w$v))) * sqrt(252); mdd <- min(w$v / cummax(w$v) - 1); dp <- if (f$currency == "VND") 0 else 2
      div(class = "fms-lp-stats",
        stat_tile("Return", fmt_pct(r), value_class = cls(r)), stat_tile("Volatility (ann.)", fmt_pct(vol, 1, FALSE)),
        stat_tile("Max drawdown", fmt_pct(mdd, 1), value_class = "down"),
        stat_tile("High / Low", paste(fmt_num(max(w$v), dp), "/", fmt_num(min(w$v), dp))))
    })
    output[[paste0(pid, "_nav")]] <- plotly::renderPlotly({
      w <- window(); cmp <- nz(input[[paste0(pid, "_cmp")]], "none")
      p <- plotly::plot_ly(x = w$date, y = w$v, type = "scatter", mode = "lines", name = code, line = list(color = SERIES[3], width = 2),
                           hovertemplate = "%{y:,.2f}<extra></extra>")
      if (cmp == "none") return(fms_plot_layout(p, legend = FALSE))
      # The benchmark is scaled to start at the fund's NAV on the first day both have a value, so the two lines start together.
      b <- comparison_series(cmp, w$d[1])
      if (nrow(b) > 1) {
        v0 <- w$v[match(b$d[1], w$d)]; if (is.na(v0)) v0 <- w$v[1]
        p <- plotly::add_lines(p, x = b$date, y = b$v / b$v[1] * v0, name = comparison_name(cmp), line = list(color = SERIES[1], width = 1.6),
                               hovertemplate = "%{y:,.2f}<extra></extra>")
      }
      fms_plot_layout(p)
    })
    output[[paste0(pid, "_comp")]] <- renderUI({
      f <- fund(); c <- composition(f); p <- c$positions; p <- p[order(-abs(p$weight)), ]
      tagList(
        layout_columns(col_widths = c(5, 7), class = "fms-lp-row",
          fms_card(tagList("Allocation by sleeve", tags$small(tr("% of NAV"))),
            html_table(source = "fms_fund_positions", c$by_sleeve, list(Sleeve = function(r) r$name, `#Weight` = function(r) fmt_pct(r$net, 1, FALSE)))),
          fms_card(tagList("By sector", tags$small(tr("% of NAV"))),
            html_table(source = "fms_fund_positions", c$by_sector, list(Sector = function(r) r$name, `#Net` = function(r) fmt_pct(r$net, 1), `#Gross` = function(r) fmt_pct(r$gross, 1, FALSE))))),
        fms_card(tagList("Positions", tags$small(sprintf(tr("%d lines"), nrow(p)))),
          html_table(source = "fms_fund_positions", p, list(Instrument = function(r) tags$b(r$contract), Description = function(r) r$description,
                     Sector = function(r) r$sector, Sleeve = function(r) r$sleeve, `#% NAV` = function(r) tags$b(class = if (r$weight < 0) "down", fmt_pct(r$weight, 2, FALSE))))))
    })
    output[[paste0(pid, "_perf")]] <- renderUI({
      f <- fund(); s <- fund_series(f$fund_id); m <- fund_metrics(f$fund_id)
      bc <- bench(); b <- comparison_series(bc, s$d[1]); bm <- if (nrow(b) > 2) metrics(b) else NULL
      src <- if (nrow(benchmark_series(bc, s$d[1]))) "fms_benchmark" else "fms_market_prices"
      rows <- data.frame(label = c("1 month", "3 months", "6 months", "Year to date", "Rolling 12 months", "Rolling 24 months, annualised", "Rolling 36 months, annualised"),
                         key = c("m1", "m3", "m6", "ytd", "m12", "m24a", "m36a"))
      layout_columns(col_widths = c(5, 7), class = "fms-lp-row",
        fms_card(tagList("Returns", tags$small(sprintf(tr("vs. %s"), comparison_name(bc)))),
          html_table(rows, list(Period = function(r) r$label, `#Fund` = function(r) tags$b(pct_cell(m[[r$key]])),
                                `#Index` = function(r) pct_cell(bm[[r$key]]), `#Diff.` = function(r) pct_cell(m[[r$key]] - bm[[r$key]]))),
          rds_source("fms_funds_prices", src)),
        fms_card(tagList("Fund vs. index", tags$small("rebased to 100")), plotly::plotlyOutput(paste0(pid, "_vs"), height = "330px"), rds_source("fms_funds_prices", src)))
    })
    # Documents: the fund's factsheets, KID, prospectus and reports; View opens the same preview as the fund page.
    output[[paste0(pid, "_docs")]] <- renderUI({
      f <- fund(); d <- q("SELECT * FROM fms_fund_documents WHERE fund_id = ? ORDER BY published_date DESC", f$fund_id)
      div(class = "fms-lp-row", fms_card(tagList("Documents", tags$small(sprintf(tr("%d published for %s"), nrow(d), f$fund_code))),
        html_table(source = "fms_fund_documents", d, list(Type = function(r) pill(r$doc_type), Title = function(r) tags$b(r$title), Published = function(r) fmt_date(r$published_date),
                   `#Pages` = function(r) r$pages, Language = function(r) r$language,
                   ` ` = function(r) tags$button(class = "btn btn-sm btn-outline-secondary", `data-action` = "doc", `data-id` = r$document_id, icon("file-lines"), "View")))))
    })
    output[[paste0(pid, "_vs")]] <- plotly::renderPlotly(fund_chart(code, bench()))
  }

  # NAV against the chosen benchmark, since the benchmark's history starts, both rebased to 100.
  fund_chart <- function(code, bc) {
    f <- q1("SELECT * FROM fms_fund_list WHERE fund_code = ?", code); req(f); s <- fund_series(f$fund_id)
    b <- comparison_series(bc, s$d[1])
    from <- if (nrow(b)) max(s$d[1], b$d[1]) else s$d[1]
    s <- s[s$d >= from, ]; b <- b[b$d >= from, ]
    p <- plotly::plot_ly() |>
      plotly::add_lines(x = s$date, y = s$v / s$v[1] * 100, name = f$fund_code, line = list(color = SERIES[2], width = 2.4), hovertemplate = "%{y:.2f}")
    if (nrow(b)) p <- plotly::add_lines(p, x = b$date, y = b$v / b$v[1] * 100, name = comparison_name(bc), line = list(color = SERIES[1], width = 1.6), hovertemplate = "%{y:.2f}")
    fms_plot_layout(p)
  }

  # Default benchmarks on the Performance tab (bench0): VN30 Index for Vietnam, S&P 500 for Commodity.
  # AllStar Vietnam Fund: Vietnam Allstar Protective Shared Fund (VND), fund code VAS-VND.
  landing_page("allstar", "VAS-VND", "AllStar", "Vietnam Fund", bench0 = "VN30")
  # AllStar Commodity Fund: Global Dynamic Commodities All Multi-Asset Top 10 Futures (USD), fund code GDC-ALL10.
  landing_page("commodity", "GDC-ALL10", "AllStar", "Commodity Fund", bench0 = "SPX")
}

LP_PERIODS <- c("1M", "3M", "6M", "YTD", "1Y", "3Y", "5Y", All = "All")
