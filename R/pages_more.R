# Extra pages added to the menu: Transactions, NAV & Pricing, Risk & Exposure, Compare funds, Documents library, Dividends, Commissions,
# Compliance, Audit log, FX rates, Reports and Calendar.

more_server <- function(input, output, session, st, go, me, act) {
  allow <- function(roles) { u <- me(); req(u); if (!u$role %in% roles) { showNotification("Your role does not have access to this area.", type = "error"); req(FALSE) }; u }

  # ---------- Transactions: every investor subscription and redemption, firm-wide
  TX_ROLES <- c("superadmin", "fund_manager")
  TX_TYPES <- c(subscription = "Subscription", redemption = "Redemption", dividend_reinvest = "Dividend reinvested")
  inflow <- function(type) type %in% c("subscription", "dividend_reinvest")
  tx_all <- reactive({
    st$tick
    t <- investor_transactions("1 = 1")
    t$date <- ifelse(is.na(t$trade_date), t$order_date, t$trade_date)
    fx <- fx_latest()
    # Pending redemptions have units but no amount yet: value them at the latest NAV.
    nav <- q("SELECT fund_id, nav_per_unit FROM fms_funds_prices WHERE price_date = ?", latest_date())
    est <- ifelse(is.na(t$amount), t$units * nav$nav_per_unit[match(t$fund_id, nav$fund_id)], t$amount)
    t$usd <- to_usd(est, t$currency, fx)
    t
  })

  output$pg_transactions <- renderUI({
    allow(TX_ROLES)
    t <- isolate(tx_all())
    funds <- sort(unique(t$fund_code))
    tagList(
      page_head("Transactions", "Every investor subscription and redemption across all funds. Amounts are also shown in USD at today's rates."),
      div(class = "fms-filter-bar",
          selectInput("tx_fund", "Fund", c(setNames("", tr("All funds")), funds), width = "200px"),
          selectInput("tx_type", "Type", c(setNames("", tr("All types")), setNames(names(TX_TYPES), tr(unname(TX_TYPES)))), width = "180px"),
          selectInput("tx_status", "Status", c(setNames("", tr("All statuses")), setNames(c("pending", "settled", "rejected", "cancelled"), tr(c("Pending", "Settled", "Rejected", "Cancelled")))), width = "180px"),
          selectInput("tx_period", "Period", setNames(c("all", "30", "90", "365"), tr(c("All time", "Last 30 days", "Last 90 days", "Last 12 months"))), width = "180px"),
          textInput("tx_search", "Investor", placeholder = "Name or e-mail", width = "220px")),
      uiOutput("tx_tiles"),
      fms_card(tagList("Monthly flows", tags$small("USD equivalent, settled orders")), plotly::plotlyOutput("tx_chart", height = "280px")),
      uiOutput("tx_table"))
  })

  tx_view <- reactive({
    t <- tx_all()
    if (nzchar(nz(input$tx_fund, ""))) t <- t[t$fund_code == input$tx_fund, ]
    if (nzchar(nz(input$tx_type, ""))) t <- t[t$tx_type == input$tx_type, ]
    if (nzchar(nz(input$tx_status, ""))) t <- t[t$status == input$tx_status, ]
    p <- nz(input$tx_period, "all")
    if (p != "all") t <- t[as.Date(t$date) >= as.Date(latest_date()) - as.integer(p), ]
    s <- tolower(trimws(nz(input$tx_search, "")))
    if (nzchar(s)) t <- t[grepl(s, tolower(paste(t$investor_name, t$investor_email)), fixed = TRUE), ]
    t
  })

  output$tx_tiles <- renderUI({
    allow(TX_ROLES); t <- tx_view(); ok <- t$status == "settled"
    subs <- sum(t$usd[ok & inflow(t$tx_type)], na.rm = TRUE)
    reds <- sum(t$usd[ok & t$tx_type == "redemption"], na.rm = TRUE)
    pend <- t$status == "pending"
    div(class = "fms-tiles",
        stat_tile("Subscriptions", fmt_compact(subs, "USD "), sprintf(tr("%d settled, incl. reinvested dividends"), sum(ok & inflow(t$tx_type)))),
        stat_tile("Redemptions", fmt_compact(reds, "USD "), sprintf(tr("%d settled"), sum(ok & t$tx_type == "redemption"))),
        stat_tile("Net flows", fmt_compact(subs - reds, "USD "), "Subscriptions less redemptions", cls(subs - reds)),
        stat_tile("Waiting for approval", sum(pend), fmt_compact(sum(t$usd[pend], na.rm = TRUE), "USD ")))
  })

  output$tx_chart <- plotly::renderPlotly({
    allow(TX_ROLES); t <- tx_view(); t <- t[t$status == "settled", ]
    validate(need(nrow(t) > 0, tr("No settled orders for these filters.")))
    t$month <- substr(t$date, 1, 7)
    months <- sort(unique(t$month))
    sum_by <- function(type) vapply(months, function(m) sum(t$usd[t$month == m & (if (type == "subscription") inflow(t$tx_type) else t$tx_type == type)], na.rm = TRUE), 0)
    x <- as.Date(paste0(months, "-01"))
    plotly::plot_ly() |>
      plotly::add_bars(x = x, y = sum_by("subscription"), name = tr("Subscriptions"), marker = list(color = "#22c38e")) |>
      plotly::add_bars(x = x, y = -sum_by("redemption"), name = tr("Redemptions"), marker = list(color = "#f2566b")) |>
      fms_plot_layout(hovermode = "x unified") |> plotly::layout(barmode = "relative", bargap = 0.35)
  })

  output$tx_table <- renderUI({
    allow(TX_ROLES); t <- tx_view()
    fms_card(tagList("Orders", tags$small(sprintf(tr("%d shown"), nrow(t)))), html_table(source = "fms_transactions", t, list(
      Date = function(r) fmt_date(r$date),
      Investor = function(r) div(tags$b(r$investor_name), tags$small(class = "muted d-block", r$investor_email)),
      Fund = function(r) fund_link(r$fund_code),
      Type = function(r) pill(TX_TYPES[[r$tx_type]], if (inflow(r$tx_type)) "good" else "bad"),
      `#Amount` = function(r) if (!is.na(r$amount)) paste(fmt_money(r$amount, r$currency), r$currency) else "–",
      `#Units` = function(r) fmt_num(r$units, 4),
      `#NAV` = function(r) fmt_num(r$nav_per_unit, if (r$currency == "VND") 0 else 4),
      `#Fee` = function(r) if (!is.na(r$fee) && r$fee > 0) fmt_money(r$fee, r$currency) else "–",
      `#USD` = function(r) fmt_compact(r$usd),
      Status = function(r) status_pill(r$status)), empty = "No orders match these filters."))
  })

  # ---------- NAV & Pricing: official NAV per fund for a valuation date, its history and today's iNAV
  output$pg_nav <- renderUI({
    allow(STAFF)
    dates <- q("SELECT DISTINCT price_date FROM fms_funds_prices ORDER BY price_date DESC LIMIT 400")$price_date
    funds <- q("SELECT fund_code, fund_name FROM fms_fund_list ORDER BY fund_id")
    tagList(
      page_head("NAV & Pricing", "Official net asset value per fund for each valuation date, with the change from the previous valuation and today's indicative NAV."),
      div(class = "fms-filter-bar",
          selectInput("nav_date", "Valuation date", setNames(dates, vapply(dates, fmt_date, "")), width = "200px"),
          selectInput("nav_fund", "Fund for the charts", funds$fund_code, width = "200px")),
      uiOutput("nav_tiles"),
      uiOutput("nav_table"),
      layout_columns(col_widths = c(7, 5),
        fms_card(tagList("NAV per unit", tags$small("last 12 months")), plotly::plotlyOutput("nav_hist", height = "280px")),
        fms_card(tagList("Intraday indicative NAV", tags$small("times in ICT (UTC+7)")), plotly::plotlyOutput("nav_intra", height = "280px"))))
  })

  nav_rows <- reactive({
    d <- nz(input$nav_date, latest_date())
    prev <- q1("SELECT MAX(price_date) AS d FROM fms_funds_prices WHERE price_date < ?", d)$d
    x <- q("SELECT f.fund_id, f.fund_code, f.fund_name, f.currency, p.nav_per_unit, p.total_nav, p.units_outstanding, b.nav_per_unit AS prev_nav, b.units_outstanding AS prev_units
            FROM fms_fund_list f JOIN fms_funds_prices p ON p.fund_id = f.fund_id AND p.price_date = ?
            LEFT JOIN fms_funds_prices b ON b.fund_id = f.fund_id AND b.price_date = ? ORDER BY f.fund_id", d, nz(prev, d))
    x$chg <- x$nav_per_unit / x$prev_nav - 1
    x$usd <- to_usd(x$total_nav, x$currency, fx_latest())
    attr(x, "prev") <- prev
    x
  })

  output$nav_tiles <- renderUI({
    allow(STAFF); x <- nav_rows()
    up <- sum(x$chg > 0, na.rm = TRUE); dn <- sum(x$chg < 0, na.rm = TRUE)
    div(class = "fms-tiles",
        stat_tile("Funds valued", nrow(x), sprintf(tr("on %s"), fmt_date(nz(input$nav_date, latest_date())))),
        stat_tile("Total net assets", fmt_compact(sum(x$usd), "USD "), "USD equivalent"),
        stat_tile("Up / down", paste(up, "/", dn), sprintf(tr("vs %s"), fmt_date(nz(attr(x, "prev"), "")))),
        stat_tile("Average change", fmt_pct(mean(x$chg, na.rm = TRUE)), "Equal-weighted", cls(mean(x$chg, na.rm = TRUE))))
  })

  output$nav_table <- renderUI({
    allow(STAFF); x <- nav_rows()
    dig <- function(ccy) if (ccy == "VND") 0 else 4
    fms_card(tagList("Net asset values", tags$small(fmt_date(nz(input$nav_date, latest_date())))), html_table(source = "fms_funds_prices", x, list(
      Fund = function(r) div(fund_link(r$fund_code, r$fund_name, "performance")),
      Cur = function(r) r$currency,
      `#NAV per unit` = function(r) fmt_num(r$nav_per_unit, dig(r$currency)),
      `#Previous` = function(r) fmt_num(r$prev_nav, dig(r$currency)),
      `#Change` = function(r) pct_cell(r$chg),
      `#Units outstanding` = function(r) fmt_num(r$units_outstanding, 2),
      `#Unit change` = function(r) fmt_num(r$units_outstanding - r$prev_units, 2),
      `#Total NAV` = function(r) fmt_money(r$total_nav, r$currency),
      `#USD` = function(r) fmt_compact(r$usd)), empty = "No valuation on this date."))
  })

  output$nav_hist <- plotly::renderPlotly({
    allow(STAFF); req(input$nav_fund)
    h <- q("SELECT p.price_date, p.nav_per_unit FROM fms_funds_prices p JOIN fms_fund_list f USING (fund_id) WHERE f.fund_code = ? AND p.price_date > date(?, '-365 day') ORDER BY p.price_date",
           input$nav_fund, latest_date())
    plotly::plot_ly(x = as.Date(h$price_date), y = h$nav_per_unit, type = "scatter", mode = "lines", name = input$nav_fund, line = list(color = SERIES[2], width = 2)) |>
      fms_plot_layout(legend = FALSE)
  })

  output$nav_intra <- plotly::renderPlotly({
    allow(STAFF); req(input$nav_fund)
    h <- q("SELECT i.ts, i.inav FROM fms_funds_intraday i JOIN fms_fund_list f USING (fund_id) WHERE f.fund_code = ? ORDER BY i.ts", input$nav_fund)
    validate(need(nrow(h) > 0, tr("No intraday prices for this fund today.")))
    t <- as.POSIXct(h$ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"); attr(t, "tzone") <- "Asia/Ho_Chi_Minh"
    plotly::plot_ly(x = t, y = h$inav, type = "scatter", mode = "lines", name = "iNAV", line = list(color = SERIES[1], width = 2)) |>
      fms_plot_layout(legend = FALSE)
  })

  # ---------- Risk & Exposure: market risk per fund, then exposure and drawdown for one fund
  risk_rows <- reactive({
    st$tick
    f <- fund_rows(me())
    do.call(rbind, lapply(seq_len(nrow(f)), function(i) {
      fi <- f[i, ]; s <- fund_series(fi$fund_id); m <- metrics(s); c <- composition(fi)
      yr <- s[s$d > add_months(m$nav_date, -12), ]; r <- diff(yr$v) / head(yr$v, -1)
      var95 <- -unname(quantile(r, 0.05, na.rm = TRUE))
      data.frame(fund_id = fi$fund_id, fund_code = fi$fund_code, fund_name = fi$fund_name, currency = fi$currency, nav = c$nav,
                 vol = m$vol_1y, mdd = m$max_drawdown, var95 = var95, var_amt = var95 * c$nav, gross = c$gross_exposure,
                 net = c$net_exposure, margin = c$margin_used / c$nav, liq = c$liquidity, sharpe = m$sharpe_1y)
    }))
  })

  output$pg_risk <- renderUI({
    allow(STAFF); x <- risk_rows()
    tagList(
      page_head("Risk & Exposure", "Market risk for every fund over the last 12 months, and where each fund's exposure sits. VaR is the 1-day loss not exceeded on 95% of days, from historical returns."),
      fms_card(tagList("Risk overview", tags$small("last 12 months")), html_table(source = c("fms_funds_prices", "fms_fund_positions"), x, list(
        Fund = function(r) fund_link(r$fund_code, r$fund_name, "composition"),
        `#Volatility` = function(r) fmt_pct(r$vol, sign = FALSE),
        `#Max drawdown` = function(r) pct_cell(r$mdd),
        `#VaR 95% 1-day` = function(r) fmt_pct(r$var95, sign = FALSE),
        `#VaR amount` = function(r) paste(fmt_compact(r$var_amt), r$currency),
        `#Gross exposure` = function(r) fmt_pct(r$gross, 0, sign = FALSE),
        `#Net exposure` = function(r) if (is.na(r$net)) "–" else fmt_pct(r$net, 0),
        `#Margin / NAV` = function(r) if (r$margin > 0) fmt_pct(r$margin, 1, sign = FALSE) else "–",
        `#Liquidity` = function(r) fmt_pct(r$liq, 0, sign = FALSE),
        `#Sharpe` = function(r) fmt_num(r$sharpe, 2)))),
      div(class = "fms-filter-bar", selectInput("rk_fund", "Fund", x$fund_code, width = "200px")),
      layout_columns(col_widths = c(6, 6),
        fms_card(tagList("Exposure by sector", tags$small("% of NAV, long and short")), plotly::plotlyOutput("rk_sector", height = "300px")),
        fms_card(tagList("Drawdown", tags$small("last 12 months")), plotly::plotlyOutput("rk_dd", height = "300px"))),
      uiOutput("rk_positions"))
  })

  rk_fund <- reactive({ req(input$rk_fund); f <- fund_by_code(input$rk_fund, me()); req(f); f })

  output$rk_sector <- plotly::renderPlotly({
    allow(STAFF); g <- composition(rk_fund())$by_sector
    g <- g[order(g$gross), ]
    y <- factor(tr(g$name), levels = tr(g$name))
    plotly::plot_ly(y = y, x = g$long * 100, type = "bar", orientation = "h", name = tr("Long"), marker = list(color = "#22c38e")) |>
      plotly::add_bars(x = g$short * 100, name = tr("Short"), marker = list(color = "#f2566b")) |>
      fms_plot_layout(ysuffix = "", hovermode = "y unified") |> plotly::layout(barmode = "relative", xaxis = list(ticksuffix = "%"), margin = list(l = 110))
  })

  output$rk_dd <- plotly::renderPlotly({
    allow(STAFF); s <- fund_series(rk_fund()$fund_id); s <- s[s$d > add_months(s$d[nrow(s)], -12), ]
    dd <- (s$v / cummax(s$v) - 1) * 100
    plotly::plot_ly(x = s$date, y = dd, type = "scatter", mode = "lines", fill = "tozeroy", name = tr("Drawdown"),
                    line = list(color = "#f2566b", width = 1.5), fillcolor = "rgba(242, 86, 107, 0.18)") |>
      fms_plot_layout(legend = FALSE) |> plotly::layout(yaxis = list(ticksuffix = "%"))
  })

  output$rk_positions <- renderUI({
    allow(STAFF); f <- rk_fund(); c <- composition(f); p <- c$positions
    p <- p[order(-abs(p$weight)), ]; p <- head(p[p$instrument_type != "cash", ], 10)
    fms_card(tagList("Largest positions", tags$small(f$fund_code)), html_table(source = "fms_fund_positions", p, list(
      Position = function(r) div(tags$b(r$contract), tags$small(class = "muted d-block", r$description)),
      Sector = function(r) r$sector, Type = function(r) pill(tools::toTitleCase(r$instrument_type)),
      `#Contracts / units` = function(r) fmt_num(r$contracts, 0),
      `#Exposure` = function(r) paste(fmt_compact(r$exposure), c$currency),
      `#% of NAV` = function(r) pct_cell(r$weight, 1),
      `#Margin` = function(r) if (r$margin > 0) fmt_compact(r$margin) else "–")))
  })

  # ---------- Compare funds: up to five funds side by side, rebased to 100 from a common start
  output$pg_compare <- renderUI({
    u <- me(); req(u); f <- fund_rows(u)
    tagList(
      page_head("Compare funds", "Pick up to five funds to compare performance, risk and costs side by side. Performance is in each fund's own currency."),
      div(class = "fms-filter-bar",
          selectizeInput("cmp_funds", "Funds", f$fund_code, selected = head(f$fund_code, 3),
                         multiple = TRUE, options = list(maxItems = 5, plugins = list("remove_button")), width = "560px"),
          selectInput("cmp_period", "Period", setNames(c("12", "36", "60", "all"), tr(c("1 year", "3 years", "5 years", "Since common start"))), width = "200px")),
      fms_card(tagList("Performance", tags$small("rebased to 100")), plotly::plotlyOutput("cmp_chart", height = "340px")),
      uiOutput("cmp_table"),
      uiOutput("cmp_corr"))
  })

  cmp_data <- reactive({
    u <- me(); req(u, length(input$cmp_funds) >= 1)
    f <- fund_rows(u); f <- f[match(input$cmp_funds, f$fund_code), ]; f <- f[!is.na(f$fund_id), ]
    ss <- lapply(f$fund_id, fund_series)
    start <- max(vapply(ss, function(s) s$d[1], ""))
    p <- nz(input$cmp_period, "12")
    if (p != "all") start <- max(start, add_months(max(vapply(ss, function(s) s$d[nrow(s)], "")), -as.integer(p)))
    ss <- lapply(ss, function(s) { s <- s[s$d >= start, ]; s$idx <- s$v / s$v[1] * 100; s })
    list(f = f, ss = ss, start = start)
  })

  output$cmp_chart <- plotly::renderPlotly({
    d <- cmp_data(); p <- plotly::plot_ly()
    for (i in seq_along(d$ss)) p <- plotly::add_lines(p, x = d$ss[[i]]$date, y = round(d$ss[[i]]$idx, 2), name = d$f$fund_code[i],
                                                      line = list(color = SERIES[(i - 1) %% length(SERIES) + 1], width = 2))
    fms_plot_layout(p)
  })

  output$cmp_table <- renderUI({
    d <- cmp_data(); f <- d$f
    specs <- q(sprintf("SELECT * FROM fms_funds_specs WHERE fund_id IN (%s)", paste(f$fund_id, collapse = ",")))
    sp <- function(id, k) specs[[k]][match(id, specs$fund_id)]
    period <- lapply(d$ss, function(s) { r <- diff(log(s$v)); list(ret = s$v[nrow(s)] / s$v[1] - 1, vol = sd(r) * sqrt(252), mdd = min(s$v / cummax(s$v) - 1)) })
    rows <- list(
      list("Currency", function(i) f$currency[i]),
      list("NAV per unit", function(i) fmt_num(f$nav[i], if (f$currency[i] == "VND") 0 else 4)),
      list("Assets under management", function(i) paste(fmt_compact(f$aum[i]), f$currency[i])),
      list("Return over the period", function(i) pct_cell(period[[i]]$ret)),
      list("Year to date", function(i) pct_cell(f$ytd[i])),
      list("12 months", function(i) pct_cell(f$m12[i])),
      list("3 years annualised", function(i) pct_cell(f$m36a[i])),
      list("Volatility over the period", function(i) fmt_pct(period[[i]]$vol, sign = FALSE)),
      list("Max drawdown over the period", function(i) pct_cell(period[[i]]$mdd)),
      list("Sharpe (1 year)", function(i) fmt_num(f$sharpe_1y[i], 2)),
      list("Risk level (1-7)", function(i) f$risk_level[i]),
      list("Management fee", function(i) fmt_pct(sp(f$fund_id[i], "management_fee_pct") / 100, sign = FALSE)),
      list("Performance fee", function(i) fmt_pct(sp(f$fund_id[i], "performance_fee_pct") / 100, 0, sign = FALSE)),
      list("Ongoing charges", function(i) fmt_pct(sp(f$fund_id[i], "ongoing_charges_pct") / 100, sign = FALSE)),
      list("Dealing", function(i) sp(f$fund_id[i], "dealing_frequency")))
    fms_card(tagList("Side by side", tags$small(sprintf(tr("period from %s"), fmt_date(d$start)))),
      tags$div(class = "table-responsive", tags$table(class = "fms-table fms-compare",
        tags$thead(tags$tr(tags$th(""), lapply(seq_len(nrow(f)), function(i) tags$th(class = "num", fund_link(f$fund_code[i]))))),
        tags$tbody(lapply(rows, function(r) tags$tr(tags$td(r[[1]]), lapply(seq_len(nrow(f)), function(i) tags$td(class = "num", r[[2]](i))))))),
      rds_source(c("fms_fund_list", "fms_funds_prices", "fms_funds_specs"))))
  })

  output$cmp_corr <- renderUI({
    d <- cmp_data(); req(length(d$ss) >= 2)
    dates <- Reduce(intersect, lapply(d$ss, `[[`, "d"))
    r <- sapply(d$ss, function(s) diff(log(s$v[match(dates, s$d)])))
    cm <- suppressWarnings(cor(r)); n <- ncol(cm); codes <- d$f$fund_code
    cell <- function(v) { a <- abs(v); bg <- if (v >= 0) sprintf("rgba(34,195,142,%.2f)", a * 0.55) else sprintf("rgba(242,86,107,%.2f)", a * 0.55)
      tags$td(class = "num", style = paste0("background:", bg), fmt_num(v, 2)) }
    fms_card(tagList("Correlation of daily returns", tags$small("1 = move together, 0 = unrelated, -1 = opposite")),
      tags$div(class = "table-responsive", tags$table(class = "fms-table fms-compare",
        tags$thead(tags$tr(tags$th(""), lapply(codes, function(cd) tags$th(class = "num", cd)))),
        tags$tbody(lapply(seq_len(n), function(i) tags$tr(tags$td(tags$b(codes[i])), lapply(seq_len(n), function(j) cell(cm[i, j])))))),
      rds_source("fms_funds_prices")))
  })

  # ---------- Documents library: every published document for the funds the user can see
  docs_all <- reactive({
    u <- me(); req(u)
    d <- q("SELECT d.*, f.fund_code, f.fund_name, f.country FROM fms_fund_documents d JOIN fms_fund_list f USING (fund_id) ORDER BY d.published_date DESC, f.fund_id")
    d[can_see(u, d$country), ]
  })

  output$pg_documents <- renderUI({
    d <- docs_all()
    types <- sort(unique(d$doc_type))
    tagList(
      page_head("Documents library", "Factsheets, key information documents, prospectuses, reports and commentaries for every fund you can access."),
      div(class = "fms-doc-types", lapply(types, function(t) tags$button(class = "fms-doc-type", `data-type` = t,
        onclick = sprintf("Shiny.setInputValue('doc_type', this.classList.toggle('on') ? '%s' : '', {priority: 'event'}); $('.fms-doc-type').not(this).removeClass('on');", t),
        tags$b(sum(d$doc_type == t)), tags$span(t)))),
      div(class = "fms-filter-bar",
          selectInput("doc_fund", "Fund", c(setNames("", tr("All funds")), unique(d$fund_code)), width = "200px"),
          textInput("doc_search", "Search", placeholder = "Title or fund", width = "260px")),
      uiOutput("doc_table"))
  })

  output$doc_table <- renderUI({
    d <- docs_all()
    if (nzchar(nz(input$doc_type, ""))) d <- d[d$doc_type == input$doc_type, ]
    if (nzchar(nz(input$doc_fund, ""))) d <- d[d$fund_code == input$doc_fund, ]
    s <- tolower(trimws(nz(input$doc_search, "")))
    if (nzchar(s)) d <- d[grepl(s, tolower(paste(d$title, d$fund_code, d$fund_name)), fixed = TRUE), ]
    fms_card(tagList("Documents", tags$small(sprintf(tr("%d shown"), nrow(d)))), html_table(source = "fms_fund_documents", d, list(
      Published = function(r) fmt_date(r$published_date),
      Fund = function(r) fund_link(r$fund_code, r$fund_name, "resources"),
      Type = function(r) pill(r$doc_type, "gold"),
      Title = function(r) tags$b(r$title),
      `#Pages` = function(r) r$pages, Language = function(r) r$language,
      ` ` = function(r) tags$button(class = "btn btn-sm btn-outline-secondary", `data-action` = "doc", `data-id` = r$document_id, icon("file-lines"), "View")),
      empty = "No documents match these filters."))
  })

  # ---------- Dividends: every declaration and what each holder received
  DIV_ROLES <- c("superadmin", "fund_manager")
  output$pg_dividends <- renderUI({
    allow(DIV_ROLES); st$tick
    d <- q("SELECT d.*, f.fund_code, f.fund_name, f.currency, u.display_name AS investor_name FROM fms_dividends d
            JOIN fms_fund_list f USING (fund_id) JOIN fms_users u ON u.user_id = d.investor_id ORDER BY d.record_date DESC, f.fund_id")
    fx <- fx_latest(); d$usd_gross <- to_usd(d$gross_amount, d$currency, fx); d$usd_net <- to_usd(d$net_amount, d$currency, fx); d$usd_tax <- to_usd(d$withholding_tax, d$currency, fx)
    decl <- unique(d[, c("fund_code", "fund_name", "currency", "record_date", "payment_date", "per_unit")])
    agg <- function(i, k) sum(d[[k]][d$fund_code == decl$fund_code[i] & d$record_date == decl$record_date[i]])
    decl$holders <- vapply(seq_len(nrow(decl)), function(i) sum(d$fund_code == decl$fund_code[i] & d$record_date == decl$record_date[i]), 0)
    decl$units <- vapply(seq_len(nrow(decl)), agg, 0, "units"); decl$gross <- vapply(seq_len(nrow(decl)), agg, 0, "gross_amount")
    decl$tax <- vapply(seq_len(nrow(decl)), agg, 0, "withholding_tax"); decl$net <- vapply(seq_len(nrow(decl)), agg, 0, "net_amount")
    tagList(
      page_head("Dividends", "Every dividend declared, what each holder received and how it was paid. Declare new dividends in Back Office."),
      div(class = "fms-tiles",
          stat_tile("Declarations", nrow(decl), sprintf(tr("%d payments to holders"), nrow(d))),
          stat_tile("Gross paid", fmt_compact(sum(d$usd_gross), "USD "), "USD equivalent"),
          stat_tile("Tax withheld", fmt_compact(sum(d$usd_tax), "USD "), "USD equivalent"),
          stat_tile("Reinvested", fmt_pct(sum(d$usd_net[d$method == "reinvest"]) / max(sum(d$usd_net), 1), 0, sign = FALSE), "of net dividends")),
      fms_card("Declarations", html_table(source = "fms_dividends", decl, list(
        Fund = function(r) fund_link(r$fund_code, r$fund_name),
        `Record date` = function(r) fmt_date(r$record_date), Payment = function(r) fmt_date(r$payment_date),
        `#Per unit` = function(r) paste(fmt_num(r$per_unit, if (r$currency == "VND") 0 else 4), r$currency),
        `#Holders` = function(r) r$holders, `#Units` = function(r) fmt_num(r$units, 2),
        `#Gross` = function(r) fmt_money(r$gross, r$currency), `#Tax` = function(r) fmt_money(r$tax, r$currency),
        `#Net` = function(r) fmt_money(r$net, r$currency)), empty = "No dividends declared yet.")),
      fms_card("Payments to holders", html_table(source = "fms_dividends", d, list(
        Payment = function(r) fmt_date(r$payment_date), Investor = function(r) tags$b(r$investor_name), Fund = function(r) tags$b(r$fund_code),
        `#Units` = function(r) fmt_num(r$units, 4), `#Gross` = function(r) fmt_money(r$gross_amount, r$currency),
        `#Tax` = function(r) fmt_money(r$withholding_tax, r$currency), `#Net` = function(r) paste(fmt_money(r$net_amount, r$currency), r$currency),
        Method = function(r) pill(if (r$method == "reinvest") "Reinvested" else "Cash", if (r$method == "reinvest") "good" else "info"),
        Status = function(r) status_pill(r$status)))))
  })

  # ---------- Commissions: introducer trail commissions, by introducer and month; superadmin marks months paid
  output$pg_commissions <- renderUI({
    allow(DIV_ROLES); st$tick; u <- me()
    c <- q("SELECT c.*, f.fund_code, f.currency, i.display_name AS introducer_name, v.display_name AS investor_name FROM fms_commissions c
            JOIN fms_fund_list f USING (fund_id) JOIN fms_users i ON i.user_id = c.introducer_id JOIN fms_users v ON v.user_id = c.investor_id
            ORDER BY c.period DESC, i.display_name")
    c$usd <- to_usd(c$amount, c$currency, fx_latest())
    key <- paste(c$introducer_id, c$period)
    m <- c[!duplicated(key), c("introducer_id", "introducer_name", "period")]
    mk <- paste(m$introducer_id, m$period)
    m$lines <- vapply(mk, function(k) sum(key == k), 0); m$basis <- vapply(mk, function(k) sum(to_usd(c$basis_amount, c$currency, fx_latest())[key == k]), 0)
    m$usd <- vapply(mk, function(k) sum(c$usd[key == k]), 0); m$accrued <- vapply(mk, function(k) any(c$status[key == k] == "accrued"), TRUE)
    acc <- sum(c$usd[c$status == "accrued"])
    tagList(
      page_head("Commissions", "Trail commissions earned by introducers on their clients' holdings, month by month, in USD equivalent."),
      div(class = "fms-tiles",
          stat_tile("Introducers", length(unique(c$introducer_id)), sprintf(tr("%d clients"), length(unique(c$investor_id)))),
          stat_tile("Paid to date", fmt_compact(sum(c$usd[c$status == "paid"]), "USD "), sprintf(tr("since %s"), min(c$period))),
          stat_tile("Accrued, not paid", fmt_compact(acc, "USD "), sprintf(tr("%d lines"), sum(c$status == "accrued")))),
      fms_card(tagList("Monthly commission", tags$small("USD equivalent")), plotly::plotlyOutput("com_chart", height = "260px")),
      fms_card("Statements", html_table(source = "fms_commissions", head(m, 36), list(
        Period = function(r) tags$b(r$period), Introducer = function(r) r$introducer_name, `#Lines` = function(r) r$lines,
        `#Basis (USD)` = function(r) fmt_compact(r$basis), `#Commission (USD)` = function(r) fmt_num(r$usd, 2),
        Status = function(r) status_pill(if (r$accrued) "accrued" else "paid"),
        ` ` = function(r) if (r$accrued && u$role == "superadmin") tags$button(class = "btn btn-sm btn-success", `data-action` = "pay_commission",
                                                                              `data-id` = paste(r$introducer_id, r$period, sep = "|"), "Mark paid")))))
  })

  output$com_chart <- plotly::renderPlotly({
    allow(DIV_ROLES); st$tick
    c <- q("SELECT c.period, c.amount, c.status, f.currency, i.display_name AS introducer FROM fms_commissions c JOIN fms_fund_list f USING (fund_id) JOIN fms_users i ON i.user_id = c.introducer_id")
    c$usd <- to_usd(c$amount, c$currency, fx_latest())
    periods <- sort(unique(c$period)); p <- plotly::plot_ly()
    for (k in seq_along(unique(c$introducer))) {
      who <- unique(c$introducer)[k]
      p <- plotly::add_bars(p, x = as.Date(paste0(periods, "-01")), y = vapply(periods, function(x) sum(c$usd[c$period == x & c$introducer == who]), 0),
                            name = who, marker = list(color = SERIES[(k - 1) %% length(SERIES) + 1]))
    }
    fms_plot_layout(p) |> plotly::layout(barmode = "stack", bargap = 0.3)
  })

  observeEvent(input$act, guard({
    if (input$act$action != "pay_commission") return()
    admin <- allow("superadmin"); parts <- strsplit(input$act$id, "|", fixed = TRUE)[[1]]
    act({
      n <- run("UPDATE fms_commissions SET status = 'paid' WHERE introducer_id = ? AND period = ? AND status = 'accrued'", as.integer(parts[1]), parts[2])
      audit(admin$user_id, "commission.pay", sprintf("Marked %s commissions paid for introducer %s", parts[2], parts[1]))
    }, "Commission marked as paid.")
  }, "commission payment"))

  # ---------- Compliance: every investment limit per fund, with headroom, plus investor file checks
  output$pg_compliance <- renderUI({
    allow(STAFF); st$tick
    ch <- compliance_checks(fund_rows(me()))
    ch$use <- ifelse(ch$kind == "max", ch$value / ch$limit, ch$limit / pmax(ch$value, 1e-9))
    ch <- ch[order(-match(ch$state, c("ok", "watch", "breach")), -ch$use), ]
    inv <- q(paste("SELECT user_id, display_name, email, id_document, bank_account, risk_profile, date_of_birth, status FROM fms_users WHERE", role_sql("investor")))
    miss <- function(r) c(if (is.na(r$id_document) || r$id_document == "") tr("ID document"), if (is.na(r$bank_account) || r$bank_account == "") tr("Bank account"),
                          if (is.na(r$risk_profile) || r$risk_profile == "") tr("Risk profile"), if (is.na(r$date_of_birth) || r$date_of_birth == "") tr("Date of birth"))
    inv$missing <- vapply(seq_len(nrow(inv)), function(i) paste(miss(inv[i, ]), collapse = ", "), "")
    bar <- function(r) { w <- min(r$use, 1.2) / 1.2 * 100
      div(class = "fms-use", div(class = paste("fms-use-fill", r$state), style = sprintf("width:%.0f%%", w)), tags$span(fmt_pct(r$use, 0, sign = FALSE))) }
    tagList(
      page_head("Compliance", "Each fund's investment limits from its prospectus, checked against current positions, and the completeness of investor files."),
      div(class = "fms-tiles",
          stat_tile("Checks", nrow(ch), sprintf(tr("%d funds"), length(unique(ch$fund_code)))),
          stat_tile("Within limits", sum(ch$state == "ok"), NULL, "up"),
          stat_tile("Near a limit", sum(ch$state == "watch"), "Within 10% of the limit"),
          stat_tile("Breaches", sum(ch$state == "breach"), NULL, if (any(ch$state == "breach")) "down" else "up")),
      fms_card(tagList("Investment limits", tags$small("use = current value against the limit")), html_table(source = c("fms_funds_specs", "fms_fund_positions"), ch, list(
        Fund = function(r) fund_link(r$fund_code, tags$b(r$fund_code), "composition"),
        Rule = function(r) r$rule,
        `#Current` = function(r) fmt_pct(r$value, 1, sign = FALSE),
        `#Limit` = function(r) paste(if (r$kind == "max") "≤" else "≥", fmt_pct(r$limit, 0, sign = FALSE)),
        Use = bar,
        Status = function(r) status_pill(r$state)))),
      fms_card(tagList("Investor files", tags$small(sprintf(tr("%d of %d complete"), sum(inv$missing == ""), nrow(inv)))), html_table(source = "fms_users", inv, list(
        Investor = function(r) div(tags$b(r$display_name), tags$small(class = "muted d-block", r$email)),
        Account = function(r) status_pill(r$status),
        Missing = function(r) if (r$missing == "") pill("Complete", "good") else tags$span(class = "fms-warn-text", r$missing)))))
  })

  # ---------- Audit log: every recorded action, searchable
  output$pg_audit <- renderUI({
    allow("superadmin"); st$tick
    a <- q("SELECT DISTINCT action FROM fms_audit_log ORDER BY action")$action
    us <- q("SELECT DISTINCT u.user_id, u.display_name FROM fms_audit_log a JOIN fms_users u USING (user_id) ORDER BY u.display_name")
    tagList(
      page_head("Audit log", "Every action recorded in FMS: sign-ins, orders, approvals, dividends, profile and user changes."),
      div(class = "fms-filter-bar",
          selectInput("au_user", "User", c(setNames("", tr("Everyone")), setNames(us$user_id, us$display_name)), width = "220px"),
          selectInput("au_action", "Action", c(setNames("", tr("All actions")), a), width = "220px"),
          selectInput("au_period", "Period", setNames(c("all", "1", "7", "30"), tr(c("All time", "Today", "Last 7 days", "Last 30 days"))), width = "180px"),
          textInput("au_search", "Search", placeholder = "Words in the detail", width = "260px")),
      uiOutput("au_table"))
  })

  output$au_table <- renderUI({
    allow("superadmin"); st$tick
    a <- q("SELECT a.*, u.display_name, u.role FROM fms_audit_log a LEFT JOIN fms_users u USING (user_id) ORDER BY a.created_at DESC, a.log_id DESC")
    if (nzchar(nz(input$au_user, ""))) a <- a[!is.na(a$user_id) & a$user_id == as.integer(input$au_user), ]
    if (nzchar(nz(input$au_action, ""))) a <- a[a$action == input$au_action, ]
    p <- nz(input$au_period, "all")
    if (p != "all") a <- a[as.Date(substr(a$created_at, 1, 10)) > Sys.Date() - as.integer(p), ]
    s <- tolower(trimws(nz(input$au_search, "")))
    if (nzchar(s)) a <- a[grepl(s, tolower(a$detail), fixed = TRUE), ]
    fms_card(tagList("Entries", tags$small(sprintf(tr("%d shown"), min(nrow(a), 500)))), html_table(source = "fms_audit_log", head(a, 500), list(
      When = function(r) fmt_datetime(r$created_at),
      User = function(r) div(tags$b(nz(r$display_name, "System")), if (!is.na(r$role)) tags$small(class = "muted d-block", ROLES[[r$role]])),
      Action = function(r) pill(r$action, if (grepl("reject|cancel", r$action)) "bad" else if (grepl("login", r$action)) "muted" else "info"),
      Detail = function(r) r$detail), empty = "No entries match these filters."))
  })

  # ---------- FX rates: the rates FMS uses to show USD totals, with a converter
  fx_hist <- reactive({
    x <- q("SELECT pair, price_date, rate FROM fms_fx_rates ORDER BY price_date")
    w <- reshape(x, idvar = "price_date", timevar = "pair", direction = "wide"); names(w) <- sub("rate.", "", names(w), fixed = TRUE)
    w$EURVND <- w$EURUSD * w$USDVND
    w[order(w$price_date), ]
  })

  output$pg_fx <- renderUI({
    req(me()); w <- fx_hist(); n <- nrow(w); last <- w$price_date[n]
    at <- function(d) { i <- max(which(w$price_date <= d)); if (is.finite(i)) i else 1 }
    pairs <- c(USDVND = "USD / VND", EURUSD = "EUR / USD", EURVND = "EUR / VND")
    card <- function(k) { v <- w[[k]]; chg <- function(i) v[n] / v[i] - 1
      div(class = "fms-stat", tags$span(class = "fms-stat-label", pairs[[k]]), tags$strong(fmt_num(v[n], if (grepl("VND", k)) 0 else 4)),
          tags$small(class = "fms-fx-chg", tr("1 day"), " ", pct_cell(chg(n - 1)), " · ", tr("1 month"), " ", pct_cell(chg(at(add_months(last, -1)))),
                     " · ", tr("YTD"), " ", pct_cell(chg(at(sprintf("%s-12-31", as.integer(substr(last, 1, 4)) - 1)))))) }
    tagList(
      page_head("FX rates", sprintf(tr("Exchange rates FMS uses to show amounts in USD, as of %s. EUR / VND is derived from the other two."), fmt_date(last))),
      div(class = "fms-tiles", lapply(names(pairs), card)),
      layout_columns(col_widths = c(8, 4),
        fms_card(tagList("History", tags$small("daily close")),
                 div(class = "fms-filter-bar", selectInput("fx_pair", NULL, setNames(names(pairs), pairs), width = "180px"),
                     selectInput("fx_span", NULL, setNames(c("3", "12", "36"), tr(c("3 months", "1 year", "3 years"))), selected = "12", width = "160px")),
                 plotly::plotlyOutput("fx_chart", height = "300px")),
        div(class = "fms-card", tags$h3("Converter"),
            numericInput("fx_amt", "Amount", 1000, min = 0, width = "100%"),
            div(class = "fms-form-grid two", selectInput("fx_from", "From", c("USD", "EUR", "VND"), width = "100%"), selectInput("fx_to", "To", c("VND", "USD", "EUR"), width = "100%")),
            uiOutput("fx_result"))))
  })

  output$fx_chart <- plotly::renderPlotly({
    req(me(), input$fx_pair); w <- fx_hist(); w <- w[w$price_date > add_months(w$price_date[nrow(w)], -as.integer(nz(input$fx_span, "12"))), ]
    plotly::plot_ly(x = as.Date(w$price_date), y = w[[input$fx_pair]], type = "scatter", mode = "lines", name = input$fx_pair, line = list(color = SERIES[2], width = 2)) |>
      fms_plot_layout(legend = FALSE)
  })

  output$fx_result <- renderUI({
    req(me()); w <- fx_hist(); r <- w[nrow(w), ]
    usd <- c(USD = 1, EUR = r$EURUSD, VND = 1 / r$USDVND)
    amt <- nz(input$fx_amt, 0); from <- nz(input$fx_from, "USD"); to <- nz(input$fx_to, "VND")
    out <- amt * usd[[from]] / usd[[to]]
    div(class = "fms-fx-result", tags$small(paste(fmt_num(amt, if (from == "VND") 0 else 2), from, "=")),
        tags$strong(paste(fmt_num(out, if (to == "VND") 0 else 2), to)),
        tags$small(class = "muted", sprintf("1 %s = %s %s", from, fmt_num(usd[[from]] / usd[[to]], if (to == "VND") 2 else 6), to)))
  })

  # ---------- Reports: downloadable files; each role sees the reports it may run
  csv_out <- function(df, file) utils::write.csv(df, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  stamp <- function() format(Sys.time(), "%Y%m%d_%H%M", tz = "Asia/Ho_Chi_Minh")
  # Cards with a fund picker span two columns, so full fund names fit on one line.
  report_card <- function(title, text, ico, ..., button) div(class = if (...length()) "fms-report wide" else "fms-report",
    div(class = "fms-report-ico", icon(ico)), div(class = "fms-report-body", tags$h4(title), tags$p(class = "muted", text), div(class = "fms-report-opts", ...)), button)

  output$pg_reports <- renderUI({
    u <- me(); req(u); f <- fund_rows(u)
    staff <- u$role %in% c("superadmin", "fund_manager")
    tagList(
      page_head("Reports", "Download reports as CSV files for Excel, or a printable fund factsheet. Each report only includes data your account can see."),
      div(class = "fms-report-grid",
        report_card("Fund factsheet", "One-page summary of a fund: performance, risk, top positions and key terms. Opens as a page you can print or save as PDF.", "file-invoice",
                    selectInput("rp_fs_fund", NULL, setNames(f$fund_code, f$fund_name), width = "100%"), button = div(class = "fms-report-btns", downloadButton("rp_factsheet", "Download", class = "btn-primary"), view_btn("factsheet"))),
        report_card("NAV history", "Daily NAV per unit, total NAV and units outstanding for one fund.", "chart-line",
                    selectInput("rp_nav_fund", NULL, setNames(f$fund_code, f$fund_name), width = "100%"),
                    selectInput("rp_nav_span", NULL, setNames(c("12", "36", "all"), tr(c("1 year", "3 years", "All history"))), width = "160px"),
                    button = div(class = "fms-report-btns", downloadButton("rp_nav", "Download CSV", class = "btn-primary"), view_btn("nav"))),
        if (u$role == "investor") report_card("My statement", "Your holdings at the latest NAV and every transaction on your account.", "wallet",
                    button = div(class = "fms-report-btns", downloadButton("rp_statement", "Download CSV", class = "btn-primary"), view_btn("statement"))),
        if (u$role == "introducer") report_card("My commission statement", "Your trail commissions by client, fund and month.", "percent",
                    button = div(class = "fms-report-btns", downloadButton("rp_mycom", "Download CSV", class = "btn-primary"), view_btn("mycom"))),
        if (staff) report_card("Transactions ledger", "All investor subscriptions, redemptions and reinvested dividends with status and amounts.", "money-bill-transfer",
                    button = div(class = "fms-report-btns", downloadButton("rp_tx", "Download CSV", class = "btn-primary"), view_btn("tx"))),
        if (u$role %in% STAFF) report_card("Positions", "Every fund's positions with exposure, weight and margin at the last valuation.", "layer-group",
                    button = div(class = "fms-report-btns", downloadButton("rp_pos", "Download CSV", class = "btn-primary"), view_btn("pos"))),
        if (staff) report_card("Dividends", "Every dividend payment to holders: gross, tax, net and method.", "hand-holding-dollar",
                    button = div(class = "fms-report-btns", downloadButton("rp_div", "Download CSV", class = "btn-primary"), view_btn("div"))),
        if (staff) report_card("Commissions", "All introducer commissions with basis, rate, amount and status.", "percent",
                    button = div(class = "fms-report-btns", downloadButton("rp_com", "Download CSV", class = "btn-primary"), view_btn("com"))),
        if (u$role == "superadmin") report_card("Users", "All accounts with role, status, country and fund access (no passwords).", "users",
                    button = div(class = "fms-report-btns", downloadButton("rp_users", "Download CSV", class = "btn-primary"), view_btn("users")))))
  })

  # Each report's rows come from one function, used by both the CSV download and the View page.
  rep_nav <- function() {
    f <- fund_by_code(input$rp_nav_fund, me()); span <- nz(input$rp_nav_span, "12")
    from <- if (span == "all") "0000" else add_months(latest_date(), -as.integer(span))
    q("SELECT price_date, nav_per_unit, total_nav, units_outstanding FROM fms_funds_prices WHERE fund_id = ? AND price_date >= ? ORDER BY price_date", f$fund_id, from)
  }
  rep_statement <- function() {
    u <- me(); h <- holdings(u$user_id)
    t <- q("SELECT t.order_date, t.trade_date, f.fund_code, t.tx_type, t.amount, t.units, t.nav_per_unit, t.fee, f.currency, t.status, t.note
            FROM fms_transactions t JOIN fms_fund_list f USING (fund_id) WHERE t.investor_id = ? ORDER BY t.order_date", u$user_id)
    rbind(
      data.frame(section = "holding", date = h$nav_date, fund = h$fund_code, type = "", amount = h$value, units = h$units, nav = h$nav, fee = NA, currency = h$currency, status = "", note = sprintf("cost %s, P&L %s", h$cost, h$pnl)),
      if (nrow(t)) data.frame(section = "transaction", date = ifelse(is.na(t$trade_date), t$order_date, t$trade_date), fund = t$fund_code, type = t$tx_type, amount = t$amount,
                              units = t$units, nav = t$nav_per_unit, fee = t$fee, currency = t$currency, status = t$status, note = nz(t$note, "")))
  }
  rep_mycom <- function() q("SELECT c.period, v.display_name AS client, f.fund_code, f.currency, c.basis_amount, c.rate_pct, c.amount, c.status
    FROM fms_commissions c JOIN fms_fund_list f USING (fund_id) JOIN fms_users v ON v.user_id = c.investor_id WHERE c.introducer_id = ? ORDER BY c.period DESC", me()$user_id)
  rep_tx <- function() { allow(DIV_ROLES); t <- tx_all()
    t[, c("transaction_id", "order_date", "trade_date", "investor_name", "investor_email", "fund_code", "tx_type", "amount", "units", "nav_per_unit", "fee", "currency", "usd", "status", "note")] }
  rep_pos <- function() { allow(STAFF); f <- fund_rows(me())
    do.call(rbind, lapply(seq_len(nrow(f)), function(i) { p <- composition(f[i, ])$positions
      data.frame(fund_code = f$fund_code[i], p[, c("contract", "description", "sector", "exchange", "instrument_type", "sleeve", "contracts", "price", "multiplier", "exposure", "weight", "margin")]) })) }
  rep_div <- function() { allow(DIV_ROLES); q("SELECT d.record_date, d.payment_date, f.fund_code, u.display_name AS investor, d.units, d.per_unit, d.gross_amount, d.withholding_tax, d.net_amount, f.currency, d.method, d.status
    FROM fms_dividends d JOIN fms_fund_list f USING (fund_id) JOIN fms_users u ON u.user_id = d.investor_id ORDER BY d.record_date DESC") }
  rep_com <- function() { allow(DIV_ROLES); q("SELECT c.period, i.display_name AS introducer, v.display_name AS client, f.fund_code, f.currency, c.basis_amount, c.rate_pct, c.amount, c.status
    FROM fms_commissions c JOIN fms_fund_list f USING (fund_id) JOIN fms_users i ON i.user_id = c.introducer_id JOIN fms_users v ON v.user_id = c.investor_id ORDER BY c.period DESC") }
  rep_users <- function() { allow("superadmin"); q("SELECT user_id, email, display_name, role, status, country, fund_access, phone, created_at, last_login_at FROM fms_users ORDER BY user_id") }

  REPORTS <- list(
    nav = list(title = "NAV history", data = rep_nav, file = function() sprintf("fms-nav-%s-%s.csv", input$rp_nav_fund, stamp()), sub = function() fund_by_code(input$rp_nav_fund, me())$fund_name),
    statement = list(title = "My statement", data = rep_statement, file = function() sprintf("fms-statement-%s.csv", stamp())),
    mycom = list(title = "My commission statement", data = rep_mycom, file = function() sprintf("fms-commissions-%s.csv", stamp())),
    tx = list(title = "Transactions ledger", data = rep_tx, file = function() sprintf("fms-transactions-%s.csv", stamp())),
    pos = list(title = "Positions", data = rep_pos, file = function() sprintf("fms-positions-%s.csv", stamp())),
    div = list(title = "Dividends", data = rep_div, file = function() sprintf("fms-dividends-%s.csv", stamp())),
    com = list(title = "Commissions", data = rep_com, file = function() sprintf("fms-commissions-all-%s.csv", stamp())),
    users = list(title = "Users", data = rep_users, file = function() sprintf("fms-users-%s.csv", stamp())))
  for (id in names(REPORTS)) local({
    r <- REPORTS[[id]]
    output[[paste0("rp_", id)]] <- downloadHandler(filename = function() r$file(), content = function(file) csv_out(r$data(), file))
  })

  view_url <- session$registerDataObj("fms_report", list(), function(data, req) {
    id <- shiny::parseQueryString(req$QUERY_STRING)$r
    shiny::withReactiveDomain(session, isolate(tryCatch({
      if (is.null(me())) stop("Please sign in again.")
      if (identical(id, "factsheet")) return(html_resp(factsheet_html()))
      r <- REPORTS[[id]]; if (is.null(r)) stop("Unknown report.")
      df <- r$data()
      sub <- paste(c(if (!is.null(r$sub)) r$sub(), sprintf(tr("%d rows"), nrow(df)), sprintf(tr("Generated %s"), format(Sys.time(), "%Y-%m-%d %H:%M", tz = "Asia/Ho_Chi_Minh"))), collapse = " · ")
      html_resp(view_page(tr(r$title), sub, view_table(df)))
    }, error = function(e) html_resp(view_page(tr("Report unavailable"), "", paste0("<p>", htmltools::htmlEscape(tr(if (nzchar(conditionMessage(e))) conditionMessage(e) else "Your role does not have access to this area.")), "</p>")), 403L))))
  })
  view_btn <- function(id) tags$a(class = "btn btn-outline-primary fms-view-btn", href = paste0(view_url, "&r=", id), target = "_blank", rel = "noopener", icon("up-right-from-square"), "View")

  # Factsheet: a self-contained HTML page in the FMS colours, ready to print.
  factsheet_html <- function() {
      u <- me(); f <- fund_by_code(input$rp_fs_fund, u); m <- fund_metrics(f$fund_id); sp <- q1("SELECT * FROM fms_funds_specs WHERE fund_id = ?", f$fund_id)
      c <- composition(f); top <- c$positions[c$positions$instrument_type != "cash", ]; top <- head(top[order(-abs(top$weight)), ], 8)
      s <- fund_series(f$fund_id); s <- s[s$d > add_months(m$nav_date, -12), ]
      x <- seq(0, 600, length.out = nrow(s)); y <- 150 - (s$v - min(s$v)) / max(diff(range(s$v)), 1e-9) * 140
      path <- paste0("M", paste(sprintf("%.1f,%.1f", x, y), collapse = " L"))
      e <- function(v) htmltools::htmlEscape(as.character(v))
      row <- function(k, v) sprintf("<tr><td>%s</td><td class='n'>%s</td></tr>", e(tr(k)), e(v))
      html <- paste0("<!doctype html><html lang='", current_lang(), "'><head><meta charset='utf-8'><title>", e(f$fund_code), " factsheet</title><style>",
        ".pbtn{position:fixed;top:16px;right:16px;background:linear-gradient(90deg,#f0c766,#c9962e);border:0;border-radius:8px;padding:8px 16px;font-weight:600;cursor:pointer}@media print{.pbtn{display:none}}",
        "body{font-family:Inter,Arial,sans-serif;color:#0d1a33;max-width:860px;margin:24px auto;padding:0 24px}h1{color:#0a6cc0;font-size:22px;text-transform:uppercase;margin:4px 0}",
        ".brand{color:#9a6a0f;font-weight:800;letter-spacing:.15em}.muted{color:#55648a}h2{font-size:13px;text-transform:uppercase;letter-spacing:.06em;color:#9a6a0f;border-bottom:2px solid #e3b453;padding-bottom:4px;margin-top:22px}",
        "table{width:100%;border-collapse:collapse;font-size:13px}td{padding:5px 6px;border-bottom:1px solid #dde4f0}.n{text-align:right}.grid{display:grid;grid-template-columns:1fr 1fr;gap:24px}",
        ".tiles{display:flex;gap:12px}.tile{flex:1;border:1px solid #dde4f0;border-radius:10px;padding:10px}.tile b{display:block;font-size:20px}.up{color:#0d8f63}.down{color:#d23a52}",
        "@media print{body{margin:0}}</style></head><body><button class='pbtn' onclick='window.print()'>", e(tr("Print")), "</button>",
        "<div class='brand'>FMS · ", e(tr("Fund factsheet")), "</div><h1>", e(f$fund_name), "</h1><div class='muted'>", e(f$fund_code), " · ISIN ", e(sp$isin), " · ",
        e(sprintf(tr("Figures as of %s"), fmt_date(m$nav_date))), "</div>",
        "<div class='tiles' style='margin-top:16px'>",
        paste(vapply(list(c("NAV per unit", paste(fmt_nav(m$nav, f$currency), f$currency), ""), c("Year to date", fmt_pct(m$ytd), cls(m$ytd)), c("12 months", fmt_pct(m$m12), cls(m$m12)),
                          c("Volatility", fmt_pct(m$vol_1y, 1, FALSE), "")), function(t) sprintf("<div class='tile'><span class='muted'>%s</span><b class='%s'>%s</b></div>", e(tr(t[1])), t[3], e(t[2])), ""), collapse = ""),
        "</div><h2>", e(tr("NAV per unit")), " · ", e(tr("last 12 months")), "</h2><svg viewBox='0 0 600 160' width='100%' height='160' preserveAspectRatio='none'><path d='", path, "' fill='none' stroke='#c9962e' stroke-width='2' vector-effect='non-scaling-stroke'/></svg>",
        "<h2>", e(tr("Objective")), "</h2><p>", e(f$objective), "</p>",
        "<div class='grid'><div><h2>", e(tr("Largest positions")), "</h2><table>", paste(sprintf("<tr><td>%s %s</td><td class='n'>%s</td></tr>", e(top$contract), e(top$description), e(fmt_pct(top$weight, 1, FALSE))), collapse = ""), "</table></div>",
        "<div><h2>", e(tr("Key facts")), "</h2><table>", row("Currency", f$currency), row("Assets under management", paste(fmt_compact(m$aum), f$currency)),
        row("Max drawdown", fmt_pct(m$max_drawdown)), row("Sharpe (1 year)", fmt_num(m$sharpe_1y, 2)), row("Risk level (1-7)", f$risk_level),
        row("Management fee", paste0(sp$management_fee_pct, "%")), row("Performance fee", paste0(sp$performance_fee_pct, "%")), row("Ongoing charges", paste0(sp$ongoing_charges_pct, "%")),
        row("Dealing", sp$dealing_frequency), row("Domicile", sp$domicile), "</table></div></div>",
        "<p class='muted' style='font-size:11px;margin-top:24px'>", sub("\\.$", "", e(sp$management_company)), ". ", e(tr("Past performance does not predict future returns. Demo data.")), "</p></body></html>")
      html
  }
  output$rp_factsheet <- downloadHandler(
    filename = function() sprintf("fms-factsheet-%s-%s.html", input$rp_fs_fund, stamp()),
    content = function(file) writeLines(factsheet_html(), file, useBytes = TRUE))

  # ---------- Calendar: dividends, documents, rebalancing, month-end and pending orders on a month grid
  is_bday <- function(d) !format(d, "%u") %in% c("6", "7")
  cal_events <- function(u, from, to) {
    ev <- list(); add <- function(date, kind, text) ev[[length(ev) + 1]] <<- data.frame(date = as.Date(date), kind = kind, text = text)
    vis <- fund_rows(u)
    d <- q("SELECT DISTINCT f.fund_code, f.country, d.record_date, d.payment_date FROM fms_dividends d JOIN fms_fund_list f USING (fund_id)")
    d <- d[can_see(u, d$country), ]
    for (i in seq_len(nrow(d))) { add(d$record_date[i], "dividend", sprintf(tr("%s dividend record date"), d$fund_code[i])); add(d$payment_date[i], "dividend", sprintf(tr("%s dividend paid"), d$fund_code[i])) }
    doc <- q("SELECT d.published_date, d.doc_type, f.fund_code, f.country FROM fms_fund_documents d JOIN fms_fund_list f USING (fund_id)")
    doc <- doc[can_see(u, doc$country), ]
    for (k in unique(paste(doc$published_date, doc$doc_type))) {
      x <- doc[paste(doc$published_date, doc$doc_type) == k, ]
      add(x$published_date[1], "document", if (nrow(x) > 1) sprintf(tr("%s published for %d funds"), tr(x$doc_type[1]), nrow(x)) else paste(x$fund_code[1], tr(x$doc_type[1])))
    }
    months <- seq(as.Date(format(from, "%Y-%m-01")), to, by = "month")
    for (m in as.list(months)) {
      days <- seq(m, seq(m, by = "month", length.out = 2)[2] - 1, by = "day"); b <- days[is_bday(days)]
      if (any(vis$country == "GLOBAL")) add(b[1], "rebalance", tr("Monthly rebalance of the global futures funds"))
      add(tail(b, 1), "valuation", tr("Month-end valuation and commission statements"))
    }
    if (u$role %in% STAFF) {
      o <- q("SELECT t.order_date, f.fund_code, t.tx_type FROM fms_transactions t JOIN fms_fund_list f USING (fund_id) WHERE t.status = 'pending'")
      for (i in seq_len(nrow(o))) { nd <- as.Date(o$order_date[i]) + 1; while (!is_bday(nd)) nd <- nd + 1
        add(nd, "order", sprintf(tr("%s %s order to deal"), o$fund_code[i], tr(o$tx_type[i]))) }
    }
    e <- do.call(rbind, ev); e <- e[e$date >= from & e$date <= to, ]
    e[order(e$date, e$kind), ]
  }
  month_label <- function(m) { y <- substr(m, 1, 4); k <- as.integer(substr(m, 6, 7))
    switch(current_lang(), vi = sprintf("Tháng %d/%s", k, y), fr = paste(MONTHS_FR[k], y), zh = sprintf("%s年%d月", y, k), paste(MONTHS_EN[k], y)) }
  CAL_KINDS <- c(dividend = "Dividends", document = "Documents", rebalance = "Rebalancing", valuation = "Month end", order = "Orders")

  output$pg_calendar <- renderUI({
    u <- me(); req(u)
    today <- as.Date(latest_date()) + 1
    months <- format(seq(as.Date(format(today, "%Y-%m-01")) - 365 * 2, as.Date(format(today, "%Y-%m-01")) + 95, by = "month"), "%Y-%m")
    months <- rev(unique(months))
    tagList(
      page_head("Calendar", "Dividends, document publications, rebalancing days, month-end valuations and orders waiting to deal."),
      div(class = "fms-filter-bar",
          selectInput("cal_month", "Month", setNames(months, vapply(months, month_label, "")),
                      selected = format(today, "%Y-%m"), width = "200px"),
          div(class = "fms-cal-legend", lapply(names(CAL_KINDS), function(k) tags$span(class = paste("fms-cal-dot", k), CAL_KINDS[[k]])))),
      layout_columns(col_widths = c(8, 4), uiOutput("cal_grid"), uiOutput("cal_upcoming")))
  })

  output$cal_grid <- renderUI({
    u <- me(); req(u, input$cal_month)
    first <- as.Date(paste0(input$cal_month, "-01")); last <- seq(first, by = "month", length.out = 2)[2] - 1
    start <- first - (as.integer(format(first, "%u")) - 1); end <- last + (7 - as.integer(format(last, "%u")))
    ev <- cal_events(u, start, end); today <- as.Date(latest_date()) + 1
    days <- seq(start, end, by = "day")
    fms_card(NULL, class = "fms-cal-card",
      div(class = "fms-cal",
          lapply(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), function(d) div(class = "fms-cal-head", d)),
          lapply(as.list(days), function(d) {
            e <- ev[ev$date == d, ]
            div(class = paste("fms-cal-day", if (format(d, "%m") != format(first, "%m")) "out", if (!is_bday(d)) "weekend", if (d == today) "today"),
                tags$span(class = "fms-cal-num", format(d, "%d")),
                lapply(seq_len(nrow(e)), function(i) div(class = paste("fms-cal-ev", e$kind[i]), title = e$text[i], e$text[i])))
          })))
  })

  output$cal_upcoming <- renderUI({
    u <- me(); req(u); today <- as.Date(latest_date()) + 1
    ev <- cal_events(u, today, today + 45)
    fms_card(tagList("Coming up", tags$small("next 45 days")),
      if (!nrow(ev)) tags$p(class = "muted", "Nothing scheduled.") else
        div(class = "fms-cal-list", lapply(seq_len(nrow(ev)), function(i) div(class = "fms-cal-item",
          tags$span(class = paste("fms-cal-dot", ev$kind[i])), div(tags$b(fmt_date(format(ev$date[i]))), tags$small(class = "d-block", ev$text[i]))))))
  })
}
