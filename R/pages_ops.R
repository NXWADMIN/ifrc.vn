# Markets monitor and the operations pages: Management, Trading, Back Office, Supervision, Users.

ops_server <- function(input, output, session, st, go, me, act) {
  allow <- function(roles) { u <- me(); req(u); if (!u$role %in% roles) { showNotification("Your role does not have access to this area.", type = "error"); req(FALSE) }; u }
  # For pages that hold form inputs: re-render on sign-in only, not after every action.
  allow_once <- function(roles) { st$user; isolate(allow(roles)) }

  # ---------- Markets (independent of the funds)
  mk <- reactive({ req(st$user); market_table() })
  mk_sel <- reactiveVal("VNINDEX")
  observeEvent(input$act, if (input$act$action == "market") mk_sel(input$act$id))
  last_fmt <- function(m) paste0(fmt_num(m$last, m$decimals), if (m$asset_class == "Rates") "%" else "")

  output$pg_markets <- renderUI({
    d <- mk()
    movers <- d[d$asset_class != "Rates", ]; movers <- movers[order(-movers$d1), ]
    mover_rows <- function(x) lapply(seq_len(nrow(x)), function(i) div(class = "fms-mover", tags$span(tags$b(x$symbol[i]), tags$span(class = "muted", x$name[i])),
                                                                    tags$span(class = "num", last_fmt(x[i, ])), tags$b(pct_cell(x$d1[i]))))
    classes <- c("Indices", "Vietnam Stocks", "International Stocks", "Commodities", "Forex", "Rates", "Crypto")
    upd <- tags$span(class = "fms-upd", tr("Last Update"), ": ", market_updated())
    tagList(
      page_head("Markets", sprintf(tr("Closing levels for indices, commodities, forex, rates and crypto, as of %s. Market data is simulated for this demo."), fmt_date(d$date[1]))),
      # Asset class buttons, then a filter box that narrows every table as you type (www/fms.js, FMS.mkApply).
      div(class = "fms-mk-tools", radioButtons("mk_class", NULL, c("All", classes), inline = TRUE),
          div(class = "fms-mk-filter", icon("magnifying-glass"),
              tags$input(id = "mk_filter_box", type = "search", class = "form-control", placeholder = tr("Filter by name, symbol or region"), oninput = "FMS.mkApply()"))),
      uiOutput("mk_tables"),
      layout_columns(col_widths = c(6, 6), fms_card("Top gainers today", actions = upd, mover_rows(head(movers, 4))), fms_card("Top losers today", actions = upd, mover_rows(head(movers[order(movers$d1), ], 4)))),
      div(id = "mk_chart_card", fms_card(uiOutput("mk_title", inline = TRUE), actions = radioButtons("mk_period", NULL, c("1M", "3M", "6M", "YTD", "1Y", "Max"), "1Y", inline = TRUE),
               plotly::plotlyOutput("mk_chart", height = "300px"))))
  })
  output$mk_title <- renderUI({
    d <- mk(); m <- d[d$symbol == mk_sel(), ]
    tagList(sprintf("%s (%s)", m$name, m$symbol), tags$small(sprintf(tr("%s · last %s %s%s"), m$region, last_fmt(m), if (m$asset_class == "Rates") "" else m$quote_unit,
                                                                     if (m$asset_class == "Rates") "" else paste(tr(" · YTD"), fmt_pct(m$ytd)))))
  })
  output$mk_chart <- plotly::renderPlotly({
    sym <- mk_sel(); per <- nz(input$mk_period, "1Y")
    s <- market_series(sym)
    last <- s$d[nrow(s)]
    from <- switch(per, `1M` = add_months(last, -1), `3M` = add_months(last, -3), `6M` = add_months(last, -6),
                   YTD = sprintf("%d-12-31", as.integer(substr(last, 1, 4)) - 1), `1Y` = add_months(last, -12), s$d[1])
    s <- s[s$d >= from, ]
    plotly::plot_ly(x = as.Date(s$d), y = s$v, type = "scatter", mode = "lines", name = sym, line = list(color = SERIES[1], width = 2)) |>
      fms_plot_layout(legend = FALSE)
  })
  output$mk_tables <- renderUI({
    d <- mk(); cl <- nz(input$mk_class, "All"); sel <- mk_sel()
    if (cl != "All") d <- d[d$asset_class == cl, ]
    ico <- c(Indices = "chart-line", `Vietnam Stocks` = "building-columns", `International Stocks` = "globe", Commodities = "oil-well", Forex = "right-left", Rates = "percent", Crypto = "bitcoin-sign")
    lapply(unique(d$asset_class), function(g) {
      x <- d[d$asset_class == g, ]
      tagList(div(class = "fms-section-head", tags$h2(g)),
        html_table(source = c("fms_market_list", "fms_market_prices"), x, class = "fms-table fms-cards-table fms-sortable", row_class = function(r) paste("fms-click", if (r$symbol == sel) "on"), list(
          Name = function(r) tags$a(href = "#", class = "fms-fund-name", `data-action` = "market", `data-id` = r$symbol,
                                     tags$span(class = "fms-fund-icon", icon(ico[[g]])), div(tags$b(r$name), tags$small(r$symbol, "·", r$quote_unit))),
          Region = function(r) r$region, `#Last` = function(r) tags$b(last_fmt(r)),
          `#1D` = function(r) if (g == "Rates") tags$b(class = cls(r$d1_abs), sprintf("%+.1f bp", r$d1_abs * 100)) else tags$b(pct_cell(r$d1)),
          `#1W` = function(r) pct_cell(r$w1), `#1M` = function(r) pct_cell(r$m1), `#YTD` = function(r) pct_cell(r$ytd), `#12M` = function(r) pct_cell(r$m12),
          `3M trend` = function(r) sparkline_svg(tail(market_series(r$symbol)$v, 63)))))
    })
  })

  # ---------- Management
  output$pg_management <- renderUI({
    allow(c("superadmin", "fund_manager")); st$tick
    f <- fund_rows(); since <- add_days(latest_date(), -30)
    f$flows_30d <- vapply(f$fund_id, function(id) {
      p <- q("SELECT units_outstanding AS u, nav_per_unit AS v FROM fms_funds_prices WHERE fund_id = ? AND price_date >= ? ORDER BY price_date", id, since)
      sum(diff(p$u) * p$v[-1])
    }, 0)
    comp <- lapply(seq_len(nrow(f)), function(i) composition(f[i, ]))
    f$gross <- vapply(comp, `[[`, 0, "gross_exposure"); f$liq <- vapply(comp, `[[`, 0, "liquidity")
    f$investors <- vapply(f$fund_id, function(id) q1("SELECT COUNT(DISTINCT investor_id) AS n FROM fms_transactions WHERE fund_id = ? AND status = 'settled'", id)$n, 0)
    flows_usd <- sum(f$flows_30d * f$aum_usd / f$aum)
    best <- f[which.max(f$ytd), ]; worst <- f[which.min(f$ytd), ]
    tagList(
      page_head("Management", "Assets, flows and positioning for every fund. Exposure for futures funds is notional gross exposure; for Vietnam funds it is the invested share."),
      div(class = "fms-tiles", stat_tile("Total AUM", fmt_compact(sum(f$aum_usd), "USD "), paste(nrow(f), "funds")),
          stat_tile("Net flows, 30 days", fmt_compact(flows_usd, "USD "), "Subscriptions less redemptions", cls(flows_usd)),
          stat_tile("Best YTD", fmt_pct(best$ytd), best$fund_code, "up"), stat_tile("Weakest YTD", fmt_pct(worst$ytd), worst$fund_code, cls(worst$ytd))),
      fms_card(tagList("Assets under management", tags$small("USD equivalent")), plotly::plotlyOutput("aum_chart", height = "320px")),
      fms_card("Funds", html_table(source = c("fms_fund_list", "fms_funds_prices"), f, list(
        Fund = function(r) tagList(fund_link(r$fund_code, tags$b(r$fund_code)), tags$span(class = "muted", r$currency)),
        `#NAV` = function(r) fmt_nav(r$nav, r$currency), `#AUM` = function(r) fmt_compact(r$aum),
        `#Flows 30d` = function(r) tags$span(class = cls(r$flows_30d), fmt_compact(r$flows_30d)), `#YTD` = function(r) pct_cell(r$ytd),
        `#12M` = function(r) pct_cell(r$m12), `#Vol 1Y` = function(r) fmt_pct(r$vol_1y, 1, FALSE), `#Exposure` = function(r) fmt_pct(r$gross, 0, FALSE),
        `#Liquidity` = function(r) fmt_pct(r$liq, 0, FALSE), `#Investors` = function(r) r$investors))))
  })
  output$aum_chart <- plotly::renderPlotly({
    f <- fund_rows(); f <- f[order(f$aum_usd), ]
    plotly::plot_ly(y = factor(f$fund_code, levels = f$fund_code), x = f$aum_usd, type = "bar", orientation = "h", marker = list(color = SERIES[1]),
                    hovertemplate = "%{y}: USD %{x:,.0f}<extra></extra>") |>
      fms_plot_layout(legend = FALSE, hovermode = "closest") |> plotly::layout(margin = list(l = 90))
  })

  # ---------- Trading
  output$pg_trading <- renderUI({
    u <- allow_once(STAFF)
    can_trade <- u$role %in% c("superadmin", "fund_trader")
    funds <- q("SELECT DISTINCT f.fund_code FROM fms_fund_positions p JOIN fms_fund_list f USING (fund_id) WHERE p.instrument_type = 'future' ORDER BY f.fund_id")$fund_code
    form <- if (can_trade) div(class = "fms-card",
      tags$h3("New order"),
      selectInput("tr_fund", "Fund", funds, width = "100%"), uiOutput("tr_contract_ui"),
      radioButtons("tr_side", NULL, c(Buy = "buy", Sell = "sell"), inline = TRUE),
      div(class = "fms-form-grid two", numericInput("tr_qty", "Contracts", 1, min = 1, step = 1), selectInput("tr_type", "Type", c(Limit = "limit", Market = "market"))),
      conditionalPanel("input.tr_type == 'limit'", numericInput("tr_limit", "Limit price", NA, width = "100%")),
      textInput("tr_note", "Note (optional)", width = "100%"),
      actionButton("tr_send", tagList(icon("right-left"), "Send order"), class = "btn-primary w-100"))
    tagList(
      page_head("Trading", paste(tr("Futures order blotter for the global funds."), if (can_trade) "Enter orders on the left; fill or cancel working orders in the blotter." else "Fund managers see the blotter read-only.")),
      if (can_trade) layout_columns(col_widths = c(3, 9), form, fms_card("Blotter", uiOutput("blotter"), actions = uiOutput("blotter_filter")))
      else fms_card("Blotter", uiOutput("blotter"), actions = uiOutput("blotter_filter")))
  })
  output$tr_contract_ui <- renderUI({
    req(input$tr_fund)
    p <- q("SELECT p.contract, p.description, p.contracts, p.price FROM fms_fund_positions p JOIN fms_fund_list f USING (fund_id) WHERE f.fund_code = ? AND p.instrument_type = 'future' ORDER BY p.contract", input$tr_fund)
    selectInput("tr_contract", "Contract", setNames(p$contract, sprintf(tr("%s · %s (pos %+d, last %s)"), p$contract, p$description, as.integer(p$contracts), fmt_num(p$price, 2))), width = "100%")
  })
  orders <- reactive({ st$tick; q("SELECT o.*, f.fund_code, u.display_name AS trader_name FROM fms_trade_orders o JOIN fms_fund_list f USING (fund_id) JOIN fms_users u ON u.user_id = o.trader_id ORDER BY o.created_at DESC") })
  output$blotter_filter <- renderUI({
    o <- orders(); n <- c(working = sum(o$status == "working"), filled = sum(o$status == "filled"), cancelled = sum(o$status == "cancelled"), all = nrow(o))
    radioButtons("bl_filter", NULL, setNames(names(n), sprintf("%s (%d)", tools::toTitleCase(names(n)), n)), isolate(nz(input$bl_filter, "working")), inline = TRUE)
  })
  output$blotter <- renderUI({
    u <- me(); req(u); o <- orders(); flt <- nz(input$bl_filter, "working")
    if (flt != "all") o <- o[o$status == flt, ]
    can_trade <- u$role %in% c("superadmin", "fund_trader")
    cols <- list(`Time (UTC)` = function(r) sub("T", " ", substr(r$created_at, 6, 16)), Fund = function(r) r$fund_code, Contract = function(r) tags$b(r$contract),
                 Side = function(r) tags$b(class = if (r$side == "buy") "up" else "down", toupper(r$side)), `#Qty` = function(r) r$contracts,
                 `#Limit` = function(r) if (r$order_type == "market") "MKT" else fmt_num(r$limit_price, 2), `#Fill` = function(r) fmt_num(r$avg_fill_price, 2),
                 Status = function(r) status_pill(r$status))
    if (u$role != "fund_trader") cols$Trader <- function(r) r$trader_name
    if (can_trade) cols$` ` <- function(r) if (r$status == "working") div(class = "fms-actions",
      tags$button(class = "btn btn-sm btn-success", `data-action` = "fill", `data-id` = r$order_id, "Fill"),
      tags$button(class = "btn btn-sm btn-outline-danger", `data-action` = "cancel_trade", `data-id` = r$order_id, "Cancel"))
    html_table(source = "fms_trade_orders", o, cols, empty = "No orders in this view.")
  })
  observeEvent(input$tr_send, act(trade_order(me(), list(fund_code = input$tr_fund, contract = input$tr_contract, side = input$tr_side, contracts = input$tr_qty,
                                                          order_type = input$tr_type, limit_price = input$tr_limit, note = input$tr_note)), "Order sent to the blotter."))
  observeEvent(input$act, guard({
    a <- input$act$action; id <- as.integer(input$act$id)
    if (a == "fill") act(trade_action(allow(c("superadmin", "fund_trader")), id, "fill"), "Order filled and position updated.")
    if (a == "cancel_trade") act(trade_action(allow(c("superadmin", "fund_trader")), id, "cancel"), "Order cancelled.")
    if (a == "approve_tx") act(process_transaction(allow("superadmin"), id, "approve"), "Order settled at the latest NAV.")
    if (a == "reject_tx") act(process_transaction(allow("superadmin"), id, "reject"), "Order rejected.")
    if (a %in% c("approve_user", "reject_user")) act(decide_signup(allow("superadmin"), id, sub("_user", "", a)), if (a == "approve_user") "Sign-up approved." else "Sign-up rejected.")
    if (a == "edit_user") user_modal(id)
  }, "row action"))

  # ---------- Back Office
  output$pg_backoffice <- renderUI({
    allow("superadmin"); st$tick
    pending <- investor_transactions("t.status = 'pending'")
    recent <- head(investor_transactions("t.status IN ('settled','rejected','cancelled')"), 25)
    divs <- q("SELECT f.fund_code, f.currency, d.record_date, MAX(d.payment_date) AS payment_date, MAX(d.per_unit) AS per_unit, MAX(d.status) AS status,
               COUNT(*) AS holders, SUM(d.net_amount) AS net FROM fms_dividends d JOIN fms_fund_list f USING (fund_id) GROUP BY f.fund_code, f.currency, d.record_date ORDER BY d.record_date DESC")
    funds <- q("SELECT fund_code, fund_name FROM fms_fund_list ORDER BY fund_id")
    tx_cols <- list(Ordered = function(r) fmt_date(r$order_date), Investor = function(r) div(tags$b(r$investor_name), tags$small(class = "muted d-block", r$investor_email)),
                    Fund = function(r) tags$b(r$fund_code), Type = function(r) tools::toTitleCase(gsub("_", " ", r$tx_type)),
                    `#Amount / units` = function(r) if (!is.na(r$amount)) paste(fmt_money(r$amount, r$currency), r$currency) else paste(fmt_num(r$units, 4), "units"))
    tagList(
      page_head("Back Office", sprintf(tr("Approve investor orders at the latest NAV (%s), and declare dividends."), fmt_date(latest_date()))),
      fms_card(tagList("Orders waiting for approval", tags$small(nrow(pending))), html_table(source = "fms_transactions", pending, c(tx_cols, list(` ` = function(r) div(class = "fms-actions",
        tags$button(class = "btn btn-sm btn-success", `data-action` = "approve_tx", `data-id` = r$transaction_id, "Approve"),
        tags$button(class = "btn btn-sm btn-outline-danger", `data-action` = "reject_tx", `data-id` = r$transaction_id, "Reject")))), empty = "No orders waiting. You're all caught up.")),
      layout_columns(col_widths = c(4, 8),
        div(class = "fms-card", tags$h3("Declare a dividend"),
            selectInput("dv_fund", "Fund", setNames(funds$fund_code, paste(funds$fund_code, "·", funds$fund_name)), width = "100%"),
            numericInput("dv_per", "Dividend per unit", NA, min = 0, width = "100%"),
            div(class = "fms-form-grid two", date_field("dv_record", "Record date", latest_date()), date_field("dv_pay", "Payment date", add_days(latest_date(), 10))),
            actionButton("dv_declare", "Declare dividend", class = "btn-primary w-100")),
        fms_card("Dividend history", html_table(source = "fms_dividends", divs, list(Fund = function(r) tags$b(r$fund_code), `Record date` = function(r) fmt_date(r$record_date),
          Payment = function(r) fmt_date(r$payment_date), `#Per unit` = function(r) paste(fmt_num(r$per_unit, if (r$currency == "VND") 0 else 2), r$currency),
          `#Holders` = function(r) r$holders, `#Net paid` = function(r) fmt_money(r$net, r$currency), Status = function(r) status_pill(r$status))))),
      fms_card("Recently processed", html_table(source = "fms_transactions", recent, c(tx_cols, list(Status = function(r) status_pill(r$status), Note = function(r) nz(r$note, ""))))))
  })
  observeEvent(input$dv_declare, act({
    n <- declare_dividend(allow("superadmin"), input$dv_fund, nz(input$dv_per), nz(input$dv_record, latest_date()), nz(input$dv_pay, add_days(nz(input$dv_record, latest_date()), 10)))
    showNotification(sprintf(tr("Dividend declared for %d holders."), n), type = "message")
  }))

  # ---------- Supervision
  output$pg_supervision <- renderUI({
    allow("superadmin"); st$tick
    f <- fund_rows(); checks <- compliance_checks(f)
    au <- q("SELECT a.*, u.display_name, u.role FROM fms_audit_log a LEFT JOIN fms_users u USING (user_id) ORDER BY a.created_at DESC LIMIT 60")
    rules <- unique(checks$rule)
    matrix <- do.call(rbind, lapply(split(checks, checks$fund_code), function(x) x[1, c("fund_code", "fund_name")]))
    matrix <- matrix[match(unique(checks$fund_code), matrix$fund_code), ]
    cell <- function(code, rule) {
      x <- checks[checks$fund_code == code & checks$rule == rule, ]
      tags$span(class = paste("fms-check", x$state), title = sprintf(tr("%s limit %s"), if (x$kind == "max") "max" else "min", fmt_pct(x$limit, 0, FALSE)),
                fmt_pct(x$value, 1, FALSE), " ", status_pill(x$state))
    }
    breaches <- sum(checks$state == "breach")
    tagList(
      page_head("Supervision", "Firm-wide oversight: assets, investor queue, compliance limits for every fund and the audit trail."),
      div(class = "fms-tiles",
          stat_tile("Assets under management", fmt_compact(sum(f$aum_usd), "USD "), paste(nrow(f), "funds")),
          stat_tile("Active investors", q1(paste("SELECT COUNT(*) AS n FROM fms_users WHERE status = 'active' AND", role_sql("investor")))$n, "With approved accounts"),
          stat_tile("Waiting for approval", sprintf(tr("%d orders · %d sign-ups"), q1("SELECT COUNT(*) AS n FROM fms_transactions WHERE status = 'pending'")$n,
                                                    q1("SELECT COUNT(*) AS n FROM fms_users WHERE status = 'pending'")$n), "Back office and Users"),
          stat_tile("Limit breaches", breaches, sprintf(tr("%d near a limit"), sum(checks$state == "watch")), if (breaches) "down" else "up")),
      fms_card(tagList("Compliance limits", tags$small("current value against each fund's limit")),
           html_table(source = c("fms_funds_specs", "fms_fund_positions"), matrix, c(list(Fund = function(r) fund_link(r$fund_code, tags$b(r$fund_code), "composition")),
                                setNames(lapply(rules, function(rl) function(r) cell(r$fund_code, rl)), rules)))),
      fms_card("Audit trail", html_table(source = "fms_audit_log", au, list(When = function(r) fmt_datetime(r$created_at), User = function(r) nz(r$display_name, "System"),
                                              Action = function(r) pill(r$action), Detail = function(r) r$detail))))
  })

  # ---------- Users (super administrator)
  output$pg_users <- renderUI({
    allow_once("superadmin")
    tagList(
      page_head("Users", "Approve sign-ups, create accounts and set each user's role and fund access.",
                actionButton("new_user", tagList(icon("user-plus"), "New user"), class = "btn-primary")),
      div(class = "fms-filters",
          radioButtons("us_status", NULL, c("All" = "all", "Pending" = "pending", "Approved" = "active", "Suspended" = "suspended", "Rejected" = "rejected"), "all", inline = TRUE),
          textInput("us_search", NULL, placeholder = "Search name or email"),
          selectInput("us_role", NULL, c("All roles" = "", setNames(names(ROLES), ROLES))),
          selectInput("us_country", NULL, c("All countries" = "", COUNTRIES))),
      uiOutput("users_table"))
  })
  output$users_table <- renderUI({
    allow("superadmin"); st$tick
    u <- q("SELECT u.*, i.display_name AS introducer_name FROM fms_users u LEFT JOIN fms_users i ON i.user_id = u.introducer_id
            ORDER BY CASE u.status WHEN 'pending' THEN 0 ELSE 1 END, u.created_at DESC")
    pend <- sum(u$status == "pending")
    if (nz(input$us_status, "all") != "all") u <- u[u$status == input$us_status, ]
    if (nz(input$us_role, "") != "") u <- u[vapply(seq_len(nrow(u)), function(i) has_role(u[i, ], input$us_role), TRUE), , drop = FALSE]
    if (nz(input$us_country, "") != "") u <- u[!is.na(u$country) & u$country == input$us_country, ]
    if (nz(input$us_search, "") != "") u <- u[grepl(tolower(input$us_search), tolower(paste(u$display_name, u$email)), fixed = TRUE), ]
    tagList(
      if (pend) div(class = "fms-alert warn", sprintf(tr("%d sign-up%s waiting for your approval."), pend, if (pend > 1 && !current_lang() %in% c("vi", "zh")) "s" else "")),
      fms_card(NULL, html_table(source = "fms_users", u, list(
        Name = function(r) div(class = "fms-person", avatar(r, 32), div(tags$b(r$display_name), tags$small(class = "muted d-block", nz(r$country, "")))),
        Email = function(r) r$email, Role = function(r) div(class = "fms-roles", lapply(roles_of(r), function(x) pill(ROLES[[x]], if (x == "superadmin") "gold" else ""))),
        Status = function(r) status_pill(if (r$status == "active") "approved" else r$status),
        `Funds access` = function(r) div(class = "fms-access", lapply(ACCESS_GROUPS, function(g) tags$span(class = if (r$role == "superadmin" || g %in% access_of(r)) "on", if (g == "GLOBAL") "Global" else "VN"))),
        Created = function(r) fmt_date(r$created_at),
        ` ` = function(r) div(class = "fms-actions",
          if (r$status == "pending") tagList(tags$button(class = "btn btn-sm btn-success", `data-action` = "approve_user", `data-id` = r$user_id, "Approve"),
                                             tags$button(class = "btn btn-sm btn-outline-danger", `data-action` = "reject_user", `data-id` = r$user_id, "Reject")),
          tags$button(class = "btn btn-sm btn-outline-secondary", `data-action` = "edit_user", `data-id` = r$user_id, icon("pen"), "Edit"))),
        empty = "No users match these filters.")))
  })

  editing <- reactiveVal(NULL)
  user_modal <- function(id = NULL) {
    allow("superadmin")
    x <- if (is.null(id)) list(role = "investor", roles = "investor", status = "active", fund_access = "GLOBAL,VIETNAM", country = "Vietnam") else user_by_id(id)
    editing(id)
    intros <- q(paste("SELECT user_id, display_name FROM fms_users WHERE status = 'active' AND", role_sql("introducer"), "ORDER BY display_name"))
    v <- function(k) nz(x[[k]], "")
    showModal(modalDialog(title = if (is.null(id)) "New user" else paste(tr("Edit"), x$display_name), size = "l", easyClose = FALSE,
      footer = tagList(modalButton("Cancel"), actionButton("um_save", if (is.null(id)) "Create user" else "Save changes", class = "btn-primary")),
      div(class = "fms-form-grid", selectInput("um_title", "Title", c("", "Mr", "Ms", "Mrs", "Dr"), v("title")), textInput("um_first", "First name", v("first_name")), textInput("um_last", "Last name", v("last_name"))),
      div(class = "fms-form-grid", textInput("um_email", "Email", v("email")), textInput("um_phone", "Phone", v("phone")), selectInput("um_country", "Country", with_current(COUNTRIES, nz(x$country)), v("country"))),
      # A user can hold several roles (e.g. investor and introducer) and switches between them from the account menu.
      checkboxGroupInput("um_roles", "Roles", setNames(names(ROLES), ROLES), roles_of(x), inline = TRUE),
      div(class = "fms-form-grid", selectInput("um_status", "Status", c(Approved = "active", Pending = "pending", Suspended = "suspended", Rejected = "rejected"), x$status),
          conditionalPanel("input.um_roles && input.um_roles.indexOf('investor') >= 0", selectInput("um_intro", "Introducer", c("None" = "", setNames(intros$user_id, intros$display_name)), v("introducer_id")))),
      checkboxGroupInput("um_access", "Funds access", c(Global = "GLOBAL", Vietnam = "VIETNAM"), access_of(x), inline = TRUE),
      passwordInput("um_password", if (is.null(id)) "Password" else "New password (leave blank to keep)", width = "100%")))
  }
  observeEvent(input$new_user, guard(user_modal(), "user form"))
  observeEvent(st$edit_user, guard({ if (!is.null(st$edit_user)) { user_modal(st$edit_user); st$edit_user <- NULL } }, "user form"))
  observeEvent(input$um_save, {
    ok <- act(save_user(allow("superadmin"), list(title = input$um_title, first_name = input$um_first, last_name = input$um_last, email = input$um_email,
                                                  phone = input$um_phone, country = input$um_country, roles = input$um_roles, status = input$um_status,
                                                  introducer_id = input$um_intro, fund_access = input$um_access, password = input$um_password), editing()),
              if (is.null(editing())) "User created." else "User updated.")
    if (ok) removeModal()
  })
}
