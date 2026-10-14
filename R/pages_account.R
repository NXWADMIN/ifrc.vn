# Personal pages: My Profile (every user) and the personal room (portfolio, clients or activity).

COUNTRIES <- c("Vietnam", "Australia", "Cambodia", "China", "France", "Germany", "Hong Kong", "Japan", "Laos", "Singapore",
               "South Korea", "Switzerland", "Thailand", "United Kingdom", "United States")
NATIONALITIES <- c("Vietnamese", "American", "Australian", "British", "Chinese", "French", "German", "Japanese", "Korean", "Singaporean", "Swiss", "Thai")
with_current <- function(list, cur) unique(c(if (!is.na(cur) && !cur %in% list) cur, list))

account_server <- function(input, output, session, st, go, me, act) {
  # ---------- My Profile
  # The photo is resized in the browser (fms.js) and arrives as a small data URL, so there is no upload bar or file name to show.
  avatar_upload <- reactiveVal(NULL)
  avatar_removed <- reactiveVal(FALSE)
  output$pg_profile <- renderUI({
    u <- isolate(me()); st$user; req(u)
    avatar_upload(NULL); avatar_removed(FALSE)
    tagList(
      page_head("My Profile", "Your personal account details. Only you can edit this profile."),
      div(class = "fms-profile",
        tags$aside(class = "fms-card fms-profile-card",
          uiOutput("profile_avatar"),
          div(class = "fms-upload",
              tags$label(class = "btn btn-outline-primary btn-sm", `for` = "pf_avatar_file", icon("camera"), "Change photo"),
              tags$input(id = "pf_avatar_file", type = "file", accept = "image/png,image/jpeg,image/webp", class = "fms-file"),
              actionButton("pf_remove", "Remove", class = "btn-link btn-sm fms-remove")),
          tags$small(class = "muted", "JPG, PNG or WEBP. Resized to 256 px."),
          uiOutput("profile_name"),
          div(class = "muted", u$email),
          div(class = "fms-profile-meta",
              pill(ROLES[[u$role]], "gold"),
              tags$dl(class = "fms-kv",
                tags$dt("Member since"), tags$dd(fmt_date(u$created_at)),
                if (!is.na(u$country)) tagList(tags$dt("Country"), tags$dd(u$country)),
                if (!is.na(u$bank_name)) tagList(tags$dt("Bank"), tags$dd(u$bank_name, tags$small(nz(u$bank_account, "")))),
                tags$dt("Fund access"), tags$dd(if (u$role == "superadmin") "All funds" else if (length(access_of(u))) paste(tr(vapply(access_of(u), title_case, "")), collapse = ", ") else "None")))),
        div(class = "fms-profile-form",
          tags$section(class = "fms-card", fms_section("Identity", "user"),
            div(class = "fms-form-grid",
                selectInput("pf_title", "Title", with_current(c("Mr", "Ms", "Mrs", "Dr"), u$title), nz(u$title, "Mr")),
                textInput("pf_first", "First name", u$first_name), textInput("pf_last", "Last name", u$last_name)),
            textInput("pf_display", "Display name", u$display_name, width = "100%")),
          tags$section(class = "fms-card", fms_section("Contact", "address-book"), div(class = "fms-form-grid",
            textInput("pf_email", "Email", u$email), textInput("pf_phone", "Phone", nz(u$phone, "")),
            selectInput("pf_country", "Country", with_current(COUNTRIES, u$country), nz(u$country, "Vietnam")))),
          tags$section(class = "fms-card", fms_section("Demographics", "id-card"), div(class = "fms-form-grid",
            date_field("pf_dob", "Birth date", nz(u$date_of_birth, NA)),
            selectInput("pf_nat", "Nationality", with_current(NATIONALITIES, u$nationality), nz(u$nationality, "Vietnamese")),
            selectInput("pf_acad", "Academic", c("Yes", "No"), nz(u$academic, "No")))),
          tags$section(class = "fms-card", fms_section("Security", "lock", "Leave blank to keep your current password."),
            passwordInput("pf_cur", "Current password", width = "100%"),
            div(class = "fms-form-grid two", passwordInput("pf_new", "New password"), passwordInput("pf_new2", "Confirm new password"))),
          div(class = "fms-save-bar", actionButton("pf_save", tagList(icon("floppy-disk"), "Save profile"), class = "btn-primary")))))
  })
  output$profile_name <- renderUI({ u <- me(); req(u); div(class = "fms-profile-name", u$display_name) })
  output$profile_avatar <- renderUI({
    u <- me(); req(u)
    if (avatar_removed()) u$avatar <- NA else if (!is.null(avatar_upload())) u$avatar <- avatar_upload()
    div(class = "fms-profile-photo", avatar(u, 112),
        if (avatar_removed() || !is.null(avatar_upload())) tags$span(class = "fms-unsaved", "Not saved yet"))
  })
  observeEvent(input$pf_avatar_data, guard({
    d <- input$pf_avatar_data$data
    if (!is.character(d) || !grepl("^data:image/(jpeg|png|webp);base64,", d) || nchar(d) > 400000) fail("That image could not be read. Try a JPG or PNG.")
    avatar_upload(d); avatar_removed(FALSE)
  }, "avatar upload"))
  observeEvent(input$pf_remove, { avatar_upload(NULL); avatar_removed(TRUE) })
  observeEvent(input$pf_save, {
    u <- me()
    ok <- act(update_profile(u, list(
      title = input$pf_title, first_name = input$pf_first, last_name = input$pf_last, display_name = input$pf_display, email = input$pf_email,
      phone = input$pf_phone, country = input$pf_country, date_of_birth = if (length(input$pf_dob)) format(input$pf_dob) else NA,
      nationality = input$pf_nat, academic = input$pf_acad, avatar = avatar_upload(), remove_avatar = avatar_removed(),
      current_password = input$pf_cur, new_password = input$pf_new, confirm_password = input$pf_new2)), "Profile saved.")
    if (ok) {
      avatar_upload(NULL); avatar_removed(FALSE)
      for (id in c("pf_cur", "pf_new", "pf_new2")) updateTextInput(session, id, value = "")
    }
  })

  # ---------- personal room
  output$pg_room <- renderUI({
    u <- me(); req(u)
    switch(u$role, investor = investor_room(u), introducer = introducer_room(u), staff_room(u))
  })

  # ---------- My Funds: the funds the signed-in investor holds, what they are worth and how each fund is doing
  output$pg_myfunds <- renderUI({
    u <- me(); req(u); st$tick
    if (u$role != "investor") return(div(class = "fms-empty", "This page is for investor accounts."))
    h <- holdings(u$user_id); total <- sum(h$value_usd)
    m <- lapply(h$fund_id, fund_metrics); h$weight <- if (total) h$value_usd / total else numeric(0)
    h$ytd <- vapply(m, function(x) nz(x$ytd, NA_real_), 0); h$m12 <- vapply(m, function(x) nz(x$m12, NA_real_), 0)
    h$d1 <- vapply(m, function(x) nz(x$d1, NA_real_), 0)
    tagList(
      page_head("My Funds", "The funds you hold today, valued at the latest NAV, and how each one is performing. Simulated demo data."),
      div(class = "fms-tiles",
          stat_tile("Funds held", nrow(h), if (nrow(h)) sprintf(tr("NAV of %s"), fmt_date(max(h$nav_date)))),
          stat_tile("Value", paste("USD", fmt_num(total)), "Converted to USD at the latest rates"),
          stat_tile("Unrealised P/L", paste("USD", fmt_num(sum(h$pnl_usd))), "Against amounts invested", cls(sum(h$pnl_usd))),
          stat_tile("Largest holding", if (nrow(h)) h$fund_code[which.max(h$value_usd)] else "None", if (nrow(h)) fmt_pct(max(h$weight), 1, FALSE))),
      if (nrow(h)) layout_columns(col_widths = c(5, 7),
        fms_card(tagList("Allocation", tags$small("share of portfolio value")), plotly::plotlyOutput("myfunds_pie", height = "280px")),
        fms_card(tagList("Value and P/L", tags$small("USD at the latest rates")), plotly::plotlyOutput("myfunds_bar", height = "280px"))),
      fms_card(tagList("Holdings", tags$small(sprintf(tr("%d shown"), nrow(h)))), html_table(source = c("fms_transactions", "fms_funds_prices", "fms_fund_list"), h, list(
        Fund = function(r) fund_link(r$fund_code, tags$b(r$fund_name)), Since = function(r) tags$span(class = "text-nowrap", fmt_date(r$first_date)),
        `#Units` = function(r) fmt_num(r$units, 4), `#NAV` = function(r) fmt_nav(r$nav, r$currency), `#Cost` = function(r) fmt_money(r$cost, r$currency),
        `#Value` = function(r) tags$b(paste(fmt_money(r$value, r$currency), r$currency)),
        `#P/L` = function(r) tags$span(class = cls(r$pnl), fmt_money(r$pnl, r$currency), tags$small(fmt_pct(r$pnl_pct))),
        `#Weight` = function(r) fmt_pct(r$weight, 1, FALSE), `#1D` = function(r) pct_cell(r$d1), `#YTD` = function(r) pct_cell(r$ytd), `#12M` = function(r) pct_cell(r$m12)),
        empty = "You don't hold any funds yet. Place a subscription in My Portfolio.")))
  })
  output$myfunds_pie <- plotly::renderPlotly({
    u <- me(); req(u); h <- holdings(u$user_id); req(nrow(h))
    plotly::plot_ly(labels = h$fund_code, values = h$value_usd, type = "pie", hole = 0.62, sort = FALSE,
                    marker = list(colors = SERIES[(seq_len(nrow(h)) - 1) %% length(SERIES) + 1], line = list(color = "#0b1630", width = 2)),
                    textinfo = "percent", hovertemplate = "%{label}: %{value:,.0f} USD<extra></extra>") |> fms_plot_layout(hovermode = "closest")
  })
  output$myfunds_bar <- plotly::renderPlotly({
    u <- me(); req(u); h <- holdings(u$user_id); req(nrow(h))
    plotly::plot_ly(x = h$fund_code) |>
      plotly::add_bars(y = h$value_usd - h$pnl_usd, name = tr("Cost"), marker = list(color = "rgba(140,160,190,0.45)"), hovertemplate = "%{y:,.0f}") |>
      plotly::add_bars(y = h$value_usd, name = tr("Value"), marker = list(color = SERIES[1]), hovertemplate = "%{y:,.0f}") |>
      fms_plot_layout() |> plotly::layout(barmode = "group", bargap = 0.35)
  })

  # ---------- My Operations: every subscription, redemption and reinvested dividend of the signed-in investor
  output$pg_myops <- renderUI({
    u <- me(); req(u); st$tick
    if (u$role != "investor") return(div(class = "fms-empty", "This page is for investor accounts."))
    t <- investor_transactions("t.investor_id = ?", u$user_id); fx <- fx_latest()
    t$date <- ifelse(is.na(t$trade_date), t$order_date, t$trade_date)
    t$usd <- to_usd(nz(t$amount, 0), t$currency, fx)
    done <- t[t$status == "settled", ]
    put_in <- sum(done$usd[done$tx_type == "subscription"]); taken <- sum(done$usd[done$tx_type == "redemption"])
    reinv <- sum(done$usd[done$tx_type == "dividend_reinvest"])
    tagList(
      page_head("My Operations", "Every time you put money into a fund or took it out, oldest at the bottom. Simulated demo data."),
      div(class = "fms-tiles",
          stat_tile("Invested", paste("USD", fmt_num(put_in)), sprintf(tr("%d subscriptions"), sum(done$tx_type == "subscription"))),
          stat_tile("Redeemed", paste("USD", fmt_num(taken)), sprintf(tr("%d redemptions"), sum(done$tx_type == "redemption"))),
          stat_tile("Net invested", paste("USD", fmt_num(put_in + reinv - taken)), "Subscriptions + reinvested dividends - redemptions", cls(put_in + reinv - taken)),
          stat_tile("Funds", length(unique(t$fund_code)), sprintf(tr("%d pending orders"), sum(t$status == "pending")))),
      if (nrow(done)) fms_card(tagList("Money in and out", tags$small("USD at the latest rates")), plotly::plotlyOutput("myops_chart", height = "260px")),
      fms_card(tagList("Operations", tags$small(sprintf(tr("%d shown"), nrow(t)))), html_table(source = c("fms_transactions", "fms_fund_list"), t, list(
        Date = function(r) tags$span(class = "text-nowrap", fmt_date(r$date)), Fund = function(r) fund_link(r$fund_code, tags$b(r$fund_name)),
        Type = function(r) pill(tools::toTitleCase(gsub("_", " ", r$tx_type)), if (r$tx_type == "redemption") "warn" else "good"),
        `#Amount` = function(r) if (!is.na(r$amount)) paste(fmt_money(r$amount, r$currency), r$currency) else "",
        `#Units` = function(r) fmt_num(r$units, 4), `#NAV` = function(r) fmt_nav(r$nav_per_unit, r$currency),
        `#Fee` = function(r) fmt_money(r$fee, r$currency), `#USD` = function(r) fmt_num(r$usd),
        Status = function(r) status_pill(r$status)), empty = "No operations yet.")))
  })
  output$myops_chart <- plotly::renderPlotly({
    u <- me(); req(u); st$tick
    t <- investor_transactions("t.investor_id = ? AND t.status = 'settled'", u$user_id); req(nrow(t)); fx <- fx_latest()
    t$usd <- to_usd(nz(t$amount, 0), t$currency, fx) * ifelse(t$tx_type == "redemption", -1, 1)
    t <- t[order(t$trade_date), ]; t$cum <- cumsum(t$usd); t$d <- as.Date(t$trade_date)
    plotly::plot_ly(t, x = ~d) |>
      plotly::add_bars(y = ~usd, name = tr("Operation"), marker = list(color = ifelse(t$usd < 0, "#f2566b", SERIES[1])), text = ~fund_code, textposition = "none", hovertemplate = "%{text}: %{y:,.0f}<extra></extra>") |>
      plotly::add_lines(y = ~cum, name = tr("Net invested"), line = list(color = SERIES[2], width = 2, shape = "hv"), hovertemplate = "%{y:,.0f}<extra></extra>") |>
      fms_plot_layout()
  })

  # ---------- My Dividends: every dividend the signed-in investor received
  output$pg_mydivs <- renderUI({
    u <- me(); req(u); st$tick
    if (u$role != "investor") return(div(class = "fms-empty", "This page is for investor accounts."))
    d <- q("SELECT d.*, f.fund_code, f.fund_name, f.currency FROM fms_dividends d JOIN fms_fund_list f USING (fund_id) WHERE investor_id = ? ORDER BY payment_date DESC", u$user_id)
    fx <- fx_latest(); d$net_usd <- to_usd(d$net_amount, d$currency, fx); paid <- d[d$status == "paid", ]
    tagList(
      page_head("My Dividends", "Each dividend you received: which fund paid it, when, and whether it was paid in cash or reinvested. Simulated demo data."),
      div(class = "fms-tiles",
          stat_tile("Received, net", paste("USD", fmt_num(sum(paid$net_usd))), "After 5% withholding tax, at the latest rates"),
          stat_tile("Payments", nrow(paid), sprintf(tr("from %d funds"), length(unique(paid$fund_code)))),
          stat_tile("Last payment", if (nrow(paid)) fmt_date(paid$payment_date[1]) else "None", if (nrow(paid)) paid$fund_code[1]),
          stat_tile("Reinvested", sum(paid$method == "reinvest"), "Paid as new units instead of cash")),
      if (nrow(paid)) fms_card(tagList("Net dividends by year", tags$small("USD at the latest rates")), plotly::plotlyOutput("mydivs_chart", height = "240px")),
      fms_card(tagList("Dividends", tags$small(sprintf(tr("%d shown"), nrow(d)))), html_table(source = c("fms_dividends", "fms_fund_list"), d, list(
        Paid = function(r) tags$span(class = "text-nowrap", fmt_date(r$payment_date)), `Record date` = function(r) tags$span(class = "text-nowrap", fmt_date(r$record_date)), Fund = function(r) fund_link(r$fund_code, tags$b(r$fund_name)),
        `#Units` = function(r) fmt_num(r$units, 4), `#Per unit` = function(r) fmt_num(r$per_unit, if (r$currency == "VND") 0 else 4),
        `#Gross` = function(r) fmt_money(r$gross_amount, r$currency), `#Tax 5%` = function(r) fmt_money(r$withholding_tax, r$currency),
        `#Net` = function(r) tags$b(paste(fmt_money(r$net_amount, r$currency), r$currency)), `#Net (USD)` = function(r) fmt_num(r$net_usd),
        Method = function(r) pill(tools::toTitleCase(r$method), if (r$method == "reinvest") "info" else "gold"), Status = function(r) status_pill(r$status)),
        empty = "No dividends yet. Distributing funds pay them once a year.")))
  })
  output$mydivs_chart <- plotly::renderPlotly({
    u <- me(); req(u)
    d <- q("SELECT d.payment_date, d.net_amount, f.fund_code, f.currency FROM fms_dividends d JOIN fms_fund_list f USING (fund_id) WHERE investor_id = ? AND d.status = 'paid'", u$user_id); req(nrow(d))
    d$usd <- to_usd(d$net_amount, d$currency, fx_latest()); d$year <- substr(d$payment_date, 1, 4)
    a <- aggregate(usd ~ year + fund_code, d, sum); p <- plotly::plot_ly()
    for (i in seq_along(unique(a$fund_code))) { k <- unique(a$fund_code)[i]; x <- a[a$fund_code == k, ]
      p <- plotly::add_bars(p, x = x$year, y = x$usd, name = k, marker = list(color = SERIES[(i - 1) %% length(SERIES) + 1]), hovertemplate = "%{y:,.0f}") }
    p |> fms_plot_layout() |> plotly::layout(barmode = "stack", bargap = 0.45)
  })

  investor_room <- function(u) {
    h <- holdings(u$user_id)
    intro <- if (!is.na(u$introducer_id)) user_by_id(u$introducer_id)
    tx <- investor_transactions("t.investor_id = ?", u$user_id)
    dv <- q("SELECT d.*, f.fund_code, f.fund_name, f.currency FROM fms_dividends d JOIN fms_fund_list f USING (fund_id) WHERE investor_id = ? ORDER BY record_date DESC", u$user_id)
    funds <- q("SELECT f.fund_code, f.fund_name, f.currency, f.country, s.min_initial_investment, s.min_subsequent, s.subscription_fee_pct, s.cut_off_time
                FROM fms_fund_list f JOIN fms_funds_specs s USING (fund_id) WHERE f.status = 'open' ORDER BY f.fund_id")
    funds <- funds[can_see(u, funds$country), ]
    pending <- tx[tx$status == "pending", ]
    tagList(
      page_head("My Portfolio", sprintf(tr("Holdings, transactions and dividends for %s."), u$display_name)),
      div(class = "fms-tiles",
          stat_tile("Portfolio value", paste("USD", fmt_num(sum(h$value_usd))), "Converted to USD at the latest rates"),
          stat_tile("Unrealised P/L", paste("USD", fmt_num(sum(h$pnl_usd))), "Against amounts invested", cls(sum(h$pnl_usd))),
          stat_tile("Dividends received", nrow(dv[dv$status == "paid", ]), "Payments to date"),
          stat_tile("Introducer", if (!is.null(intro)) intro$display_name else "None", if (!is.null(intro)) intro$email)),
      navset_card_underline(id = "room_tab", selected = isolate(input$room_tab),
        nav_panel("Portfolio", html_table(source = c("fms_transactions", "fms_funds_prices"), h, list(
          Fund = function(r) fund_link(r$fund_code, tagList(tags$b(r$fund_code), tags$small(r$fund_name))),
          `#Units` = function(r) fmt_num(r$units, 4), `#NAV` = function(r) fmt_nav(r$nav, r$currency), `#Cost` = function(r) fmt_money(r$cost, r$currency),
          `#Value` = function(r) tags$b(fmt_money(r$value, r$currency)), Cur = function(r) r$currency,
          `#P/L` = function(r) tags$span(class = cls(r$pnl), fmt_money(r$pnl, r$currency), tags$small(fmt_pct(r$pnl_pct))),
          `#Value (USD)` = function(r) fmt_num(r$value_usd)), empty = "You don't hold any funds yet. Place a subscription in New order.")),
        nav_panel("Transactions", html_table(source = "fms_transactions", tx, list(
          Date = function(r) fmt_date(nz(r$trade_date, r$order_date)), Fund = function(r) tags$b(r$fund_code), Type = function(r) tools::toTitleCase(gsub("_", " ", r$tx_type)),
          `#Amount` = function(r) paste(fmt_money(r$amount, r$currency), r$currency), `#Units` = function(r) fmt_num(r$units, 4),
          `#NAV` = function(r) fmt_nav(r$nav_per_unit, r$currency), `#Fee` = function(r) fmt_money(r$fee, r$currency),
          Status = function(r) status_pill(r$status), Note = function(r) nz(r$note, ""),
          ` ` = function(r) if (r$status == "pending") tags$button(class = "btn btn-sm btn-outline-danger", `data-action` = "cancel_order", `data-id` = r$transaction_id, "Cancel")))),
        nav_panel("Dividends", html_table(source = "fms_dividends", dv, list(
          `Record date` = function(r) fmt_date(r$record_date), Paid = function(r) fmt_date(r$payment_date), Fund = function(r) tags$b(r$fund_code),
          `#Per unit` = function(r) fmt_num(r$per_unit, if (r$currency == "VND") 0 else 4), `#Units` = function(r) fmt_num(r$units, 4),
          `#Gross` = function(r) fmt_money(r$gross_amount, r$currency), `#Tax 5%` = function(r) fmt_money(r$withholding_tax, r$currency),
          `#Net` = function(r) tags$b(paste(fmt_money(r$net_amount, r$currency), r$currency)), Method = function(r) tools::toTitleCase(r$method),
          Status = function(r) status_pill(r$status)), empty = "No dividends yet. Distributing funds pay them once a year.")),
        nav_panel("New order", layout_columns(col_widths = c(5, 7),
          div(class = "fms-box",
            selectInput("ord_fund", "Fund", setNames(funds$fund_code, paste(funds$fund_code, "·", funds$fund_name)), width = "100%"),
            radioButtons("ord_type", "Order", c(Subscribe = "subscription", Redeem = "redemption"), inline = TRUE),
            conditionalPanel("input.ord_type == 'subscription'", numericInput("ord_amount", "Amount (fund currency)", NA, min = 0, width = "100%")),
            conditionalPanel("input.ord_type == 'redemption'", numericInput("ord_units", "Units", NA, min = 0, width = "100%")),
            uiOutput("ord_hint"),
            actionButton("ord_submit", "Place order", class = "btn-primary")),
          div(tags$h4("Pending orders"), html_table(source = "fms_transactions", pending, list(
            Date = function(r) fmt_date(r$order_date), Fund = function(r) tags$b(r$fund_code), Type = function(r) tools::toTitleCase(gsub("_", " ", r$tx_type)),
            `#Amount / units` = function(r) if (!is.na(r$amount)) paste(fmt_money(r$amount, r$currency), r$currency) else paste(fmt_num(r$units, 4), "units"),
            ` ` = function(r) tags$button(class = "btn btn-sm btn-outline-danger", `data-action` = "cancel_order", `data-id` = r$transaction_id, "Cancel")),
            empty = "No pending orders. Orders are settled at the next NAV once the back office approves them."))))))
  }
  output$ord_hint <- renderUI({
    u <- me(); req(u, input$ord_fund)
    s <- q1("SELECT f.fund_id, f.currency, s.* FROM fms_fund_list f JOIN fms_funds_specs s USING (fund_id) WHERE fund_code = ?", input$ord_fund); req(s)
    held <- units_held(u$user_id, s$fund_id)
    tags$p(class = "fms-notice", if (identical(input$ord_type, "redemption")) sprintf(tr("You hold %s units."), fmt_num(held, 4))
           else sprintf(tr("Minimum %s %s %s, subscription fee up to %s%%, cut-off %s."), if (held > 0) "additional" else "initial",
                        fmt_num(if (held > 0) s$min_subsequent else s$min_initial_investment, 0), s$currency, s$subscription_fee_pct, s$cut_off_time))
  })
  observeEvent(input$ord_submit, {
    ok <- act(place_order(me(), input$ord_fund, input$ord_type, nz(input$ord_amount), nz(input$ord_units)),
              "Order placed. The back office settles it at the next NAV.")
    if (ok) { updateNumericInput(session, "ord_amount", value = NA); updateNumericInput(session, "ord_units", value = NA) }
  })
  observeEvent(input$act, if (input$act$action == "cancel_order") act(cancel_order(me(), as.integer(input$act$id)), "Order cancelled."))

  introducer_room <- function(u) {
    fx <- fx_latest()
    clients <- q("SELECT * FROM fms_users WHERE introducer_id = ? ORDER BY display_name", u$user_id)
    clients$holdings <- vapply(clients$user_id, function(id) nrow(holdings(id)), 0)
    clients$assets_usd <- vapply(clients$user_id, function(id) sum(holdings(id)$value_usd), 0)
    com <- q("SELECT c.period, c.status, f.currency, SUM(c.amount) AS amount, COUNT(*) AS lines FROM fms_commissions c JOIN fms_fund_list f USING (fund_id)
              WHERE introducer_id = ? GROUP BY c.period, c.status, f.currency ORDER BY c.period DESC", u$user_id)
    com$usd <- to_usd(com$amount, com$currency, fx)
    per <- aggregate(cbind(usd, lines) ~ period + status, com, sum)
    per <- per[order(per$period, decreasing = TRUE), ]
    ctx <- if (nrow(clients)) head(investor_transactions(sprintf("t.investor_id IN (%s)", paste(clients$user_id, collapse = ","))), 40) else NULL
    shown <- if (isTRUE(st$all_statements)) per else head(per, 12)
    tagList(
      page_head("My Clients", "Investors you introduced, their activity and your trail commissions."),
      div(class = "fms-tiles",
          stat_tile("Clients", nrow(clients), "Introduced investors"), stat_tile("Client assets", paste("USD", fmt_num(sum(clients$assets_usd))), "Converted to USD"),
          stat_tile("Commission paid", paste("USD", fmt_num(sum(per$usd[per$status == "paid"]))), "All periods"),
          stat_tile("Commission accrued", paste("USD", fmt_num(sum(per$usd[per$status == "accrued"]))), "Paid next month")),
      fms_card("Clients", html_table(source = c("fms_users", "fms_transactions"), clients, list(
        Client = function(r) div(class = "fms-person", avatar(r, 30), div(tags$b(r$display_name), tags$small(r$email))),
        Phone = function(r) nz(r$phone, "–"), Risk = function(r) nz(r$risk_profile, "–"), `#Holdings` = function(r) r$holdings,
        `#Assets (USD)` = function(r) tags$b(fmt_num(r$assets_usd)), Status = function(r) status_pill(r$status)), empty = "No clients yet.")),
      layout_columns(col_widths = c(7, 5),
        fms_card(tagList("Monthly trail commission", tags$small("USD")), plotly::plotlyOutput("commission_chart", height = "260px")),
        fms_card("Statements", html_table(source = "fms_commissions", shown, list(Period = function(r) r$period, `#Lines` = function(r) r$lines,
                                                  `#Amount (USD)` = function(r) tags$b(fmt_num(r$usd)), Status = function(r) status_pill(r$status))),
             if (nrow(per) > 12) actionLink("all_statements", if (isTRUE(st$all_statements)) "Show the last 12 only" else sprintf(tr("Show all %d statements"), nrow(per))))),
      fms_card("Client transactions", html_table(source = "fms_transactions", ctx, list(
        Date = function(r) fmt_date(nz(r$trade_date, r$order_date)), Client = function(r) r$investor_name, Fund = function(r) tags$b(r$fund_code),
        Type = function(r) tools::toTitleCase(gsub("_", " ", r$tx_type)), `#Amount` = function(r) paste(fmt_money(r$amount, r$currency), r$currency), Status = function(r) status_pill(r$status)))))
  }
  observeEvent(input$all_statements, st$all_statements <- !isTRUE(st$all_statements))
  output$commission_chart <- plotly::renderPlotly({
    u <- me(); req(u, u$role == "introducer"); fx <- fx_latest()
    com <- q("SELECT c.period, c.status, f.currency, SUM(c.amount) AS amount FROM fms_commissions c JOIN fms_fund_list f USING (fund_id)
              WHERE introducer_id = ? GROUP BY c.period, c.status, f.currency", u$user_id)
    com$usd <- to_usd(com$amount, com$currency, fx)
    per <- aggregate(usd ~ period + status, com, sum); per <- per[order(per$period), ]; per <- tail(per, 24)
    per$status <- tr(per$status)
    plotly::plot_ly(per, x = ~period, y = ~usd, color = ~status, colors = setNames(SERIES[2:1], tr(c("accrued", "paid"))), type = "bar",
                    hovertemplate = "%{x}: USD %{y:,.2f}<extra></extra>") |> fms_plot_layout(hovermode = "closest")
  })

  # My Activity: the user's own audit entries, 10 by default, more on request.
  staff_room <- function(u) {
    tagList(page_head("My Activity", "Your recent actions in FMS."),
            fms_card(uiOutput("myact_title", inline = TRUE),
                     actions = selectInput("myact_n", NULL, c("10 entries" = "10", "25 entries" = "25", "50 entries" = "50", "100 entries" = "100", "All entries" = "all"),
                                           isolate(nz(input$myact_n, "10")), width = "150px"),
                     uiOutput("myact_table")))
  }
  myact <- reactive({ u <- me(); req(u); st$tick; q("SELECT * FROM fms_audit_log WHERE user_id = ? ORDER BY created_at DESC", u$user_id) })
  output$myact_title <- renderUI({
    a <- myact(); n <- nz(input$myact_n, "10"); shown <- if (n == "all") nrow(a) else min(nrow(a), as.integer(n))
    tagList("Activity", tags$small(sprintf(tr("%d of %d entries"), shown, nrow(a))))
  })
  output$myact_table <- renderUI({
    a <- myact(); n <- nz(input$myact_n, "10"); if (n != "all") a <- head(a, as.integer(n))
    html_table(source = "fms_audit_log", a, list(When = function(r) fmt_datetime(r$created_at), Action = function(r) pill(r$action), Detail = function(r) r$detail),
               empty = "No activity yet.")
  })
}
