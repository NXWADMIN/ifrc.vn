# Sign-in, sign-up and the app shell: left menu, search, notifications and account menu.

DEMO_ACCOUNTS <- data.frame(
  email = c("superadmin@fms.io.vn", "fund.manager@fms.io.vn", "fund.trader@fms.io.vn", "demo1.investor@fms.io.vn",
            "demo2.investor@fms.io.vn", "demo3.investor@fms.io.vn", "demo1.introducer@fms.io.vn", "demo2.introducer@fms.io.vn"),
  password = c("superadmin", "fund", "trader", "demo1", "demo2", "demo3", "introducer", "introducer"),
  role = c("superadmin", "fund_manager", "fund_trader", "investor", "investor", "investor", "introducer", "introducer"))

# Options: light or dark mode and the interface language. Before sign-in they come from the browser;
# after sign-in from the user's record, and every change is saved to it.
options_server <- function(input, output, session, st, me) {
  opt_buttons <- function(key, values, labels, active = NULL) {
    div(class = "fms-opts", lapply(seq_along(values), function(i)
      tags$button(type = "button", class = paste("fms-opt", if (identical(values[i], active)) "on"), `data-opt` = key, `data-value` = values[i], labels[[i]])))
  }
  options_ui <- function() tagList(
    div(class = "fms-pop-title", "Options"),
    div(class = "fms-opt-group", tags$label("Mode"),
        opt_buttons("theme", THEMES, list(tagList(icon("moon"), "Dark"), tagList(icon("sun"), "Light")))),
    div(class = "fms-opt-group", tags$label("Language"), opt_buttons("lang", names(LANGS), unname(LANGS), st$lang)))
  output$options_menu <- renderUI(options_ui())
  output$auth_options <- renderUI(options_ui())
  outputOptions(output, "options_menu", suspendWhenHidden = FALSE)

  # Tooltips and the search placeholder live in the static top bar, so the browser relabels them.
  send_prefs <- function(theme = NULL) {
    session$sendCustomMessage("fms-prefs", list(lang = st$lang, theme = theme, labels = list(
      menu = tr("Menu"), main = tr("Main menu"), notifications = tr("Notifications"), options = tr("Options"),
      account = tr("Account"), search = tr("Search funds or users"))))
  }
  observeEvent(input$client_prefs, {
    if (is.null(st$user) && isTRUE(input$client_prefs$lang %in% names(LANGS))) st$lang <- input$client_prefs$lang
    send_prefs()
  })
  observeEvent(st$user, {
    # A choice made on the sign-in screen wins and is saved; otherwise the account's saved choice applies.
    u <- isolate(me()); req(u)
    if (isTRUE(u$lang %in% names(LANGS)) && !picked$lang) st$lang <- u$lang
    else run("UPDATE fms_users SET lang = ? WHERE user_id = ?", isolate(st$lang), u$user_id)
    keep <- isTRUE(u$theme %in% THEMES) && !picked$theme
    theme <- if (keep) u$theme else nz(isolate(input$client_prefs$theme), "dark")
    if (!keep) run("UPDATE fms_users SET theme = ? WHERE user_id = ?", theme, u$user_id)
    send_prefs(theme)
  })
  picked <- reactiveValues(lang = FALSE, theme = FALSE)
  observeEvent(input$opt, guard({
    key <- input$opt$key; v <- input$opt$value
    if (is.null(st$user) && key %in% c("lang", "theme")) picked[[key]] <- TRUE
    if (key == "lang" && v %in% names(LANGS)) {
      st$lang <- v
      if (!is.null(st$user)) run("UPDATE fms_users SET lang = ? WHERE user_id = ?", v, st$user$user_id)
      send_prefs()
    }
    if (key == "theme" && v %in% THEMES && !is.null(st$user)) run("UPDATE fms_users SET theme = ? WHERE user_id = ?", v, st$user$user_id)
  }, "options"))
}

auth_server <- function(input, output, session, st, go, me) {
  output$auth_ui <- renderUI({
    st$lang
    div(class = "fms-auth-row", div(class = "fms-auth-card",
      div(class = "fms-auth-top", div(class = "fms-brand big", tags$img(class = "fms-nxw-logo", src = "nxw-logo.png", alt = "NXW Next Wealth"),
                                                div(tags$span(class = "fms-wordmark", "FMS"), tags$small(class = "fms-tagline", "Fund Management System"))),
          div(class = "dropdown dropend fms-auth-opts", tags$button(class = "fms-icon-btn", type = "button", title = "Options", `data-bs-toggle` = "dropdown",
                                              `data-bs-auto-close` = "outside", icon("sliders")),
              div(class = "dropdown-menu fms-pop", uiOutput("auth_options")))),
      tags$p(class = "muted", "Funds Management System: supervision, management, trading and back office."),
      navset_underline(id = "auth_tab", selected = isolate(input$auth_tab),
        nav_panel("Sign in", value = "signin",
          textInput("login_email", "Email", isolate(input$login_email), placeholder = "you@fms.io.vn", width = "100%"),
          passwordInput("login_password", "Password", width = "100%"),
          checkboxInput("login_remember", "Remember me", value = !isFALSE(isolate(input$login_remember)), width = "100%"),
          uiOutput("login_error"),
          actionButton("login_btn", "Sign in", class = "btn-primary w-100"),
          # Demo accounts start folded; the + opens the list.
          tags$details(class = "fms-demo-list", tags$summary(tags$h4("Demo accounts"), tags$span(class = "fms-fold", icon("plus"))),
                       uiOutput("demo_accounts"))),
        nav_panel("Sign up", value = "signup",
          tags$p(class = "muted", "New accounts are reviewed by the administrator before you can sign in."),
          layout_columns(col_widths = c(3, 9), selectInput("su_title", "Title", c("", "Mr", "Ms", "Mrs", "Dr")),
                         radioButtons("su_role", "I am", c(Investor = "investor", Introducer = "introducer"), inline = TRUE)),
          layout_columns(textInput("su_first", "First name"), textInput("su_last", "Last name")),
          textInput("su_email", "Email", width = "100%"),
          layout_columns(textInput("su_phone", "Phone"), textInput("su_country", "Country", "Vietnam")),
          conditionalPanel("input.su_role == 'investor'", textInput("su_intro", "Introducer email (optional)", width = "100%")),
          layout_columns(passwordInput("su_password", "Password"), passwordInput("su_confirm", "Confirm password")),
          uiOutput("signup_msg"),
          actionButton("signup_btn", "Request account", class = "btn-primary w-100")))))
  })
  output$demo_accounts <- renderUI(tagList(lapply(seq_len(nrow(DEMO_ACCOUNTS)), function(i) {
    a <- DEMO_ACCOUNTS[i, ]
    tags$button(class = "fms-demo", `data-email` = a$email, `data-password` = a$password,
                tags$span(a$email), tags$small(ROLES[[a$role]], " · ", a$password))
  })))

  outputOptions(output, "demo_accounts", suspendWhenHidden = FALSE)  # rendered while folded, so it opens at once

  login_error <- reactiveVal(NULL)
  output$login_error <- renderUI(if (!is.null(login_error())) div(class = "fms-alert bad", login_error()))
  observeEvent(input$login_btn, {
    tryCatch({
      u <- login(input$login_email, input$login_password)
      login_error(NULL)
      st$user <- u
      # Remember me: stay signed in on this browser for 30 days; otherwise only until the browser is closed.
      remember <- !isFALSE(input$login_remember)
      session$userData$token <- session_start(u$user_id, if (remember) 30 else 1)
      session$sendCustomMessage("fms-token", list(token = session$userData$token, remember = remember))
      updateTextInput(session, "login_password", value = "")
      go("dashboard")
    }, fms_error = function(e) login_error(conditionMessage(e)),
       error = function(e) { report_error(e, "sign-in"); login_error(paste(tr("Sign-in failed on the server:"), conditionMessage(e))) })
  })

  signup_msg <- reactiveVal(NULL)
  output$signup_msg <- renderUI(if (!is.null(signup_msg())) div(class = paste("fms-alert", signup_msg()$kind), signup_msg()$text))
  observeEvent(input$signup_btn, {
    tryCatch({
      signup(list(title = input$su_title, role = input$su_role, first_name = input$su_first, last_name = input$su_last,
                  email = input$su_email, phone = input$su_phone, country = input$su_country, introducer_email = input$su_intro,
                  password = input$su_password, confirm_password = input$su_confirm))
      signup_msg(list(kind = "good", text = "Thanks. Your sign-up is waiting for approval by the administrator. You can sign in once it is approved."))
      for (id in c("su_first", "su_last", "su_email", "su_phone", "su_intro", "su_password", "su_confirm")) updateTextInput(session, id, value = "")
    }, fms_error = function(e) signup_msg(list(kind = "bad", text = conditionMessage(e))),
       error = function(e) { report_error(e, "sign-up"); signup_msg(list(kind = "bad", text = paste(tr("Sign-up failed on the server:"), conditionMessage(e)))) })
  })

  observeEvent(input$logout, {
    if (!is.null(st$user)) guard(audit(st$user$user_id, "user.logout", "Signed out"), "sign-out")
    guard(session_end(session$userData$token), "sign-out")
    st$user <- NULL
    session$sendCustomMessage("fms-token", "")
    session$sendCustomMessage("fms-reset", TRUE)
  })

  # After a reload or a lost connection the browser sends its sign-in token: sign back in without the password.
  observeEvent(input$resume, once = TRUE, {
    r <- input$resume
    u <- if (is.null(st$user)) tryCatch(session_resume(r$token), error = function(e) NULL)
    if (!is.null(u)) {
      st$user <- u
      session$userData$token <- r$token
    } else if (is.null(st$user)) session$sendCustomMessage("fms-token", "")
    session$sendCustomMessage("fms-resumed", !is.null(u))  # the browser then reopens the page it was on
  })
}

shell_server <- function(input, output, session, st, go, me, act) {
  counts <- reactive({ u <- me(); req(u); badge_counts(u) })

  # Horizontal menu: main pages as links, Operations and Admin as dropdowns, each shown only to roles that can use it.
  output$nav_menu <- renderUI({
    u <- me(); req(u); n <- counts()
    room_label <- switch(u$role, investor = "My Portfolio", introducer = "My Clients", "My Activity")
    badge <- function(x) if (x > 0) tags$span(class = "fms-badge", x)
    item <- function(page, label, ico, count = 0) nav_link(page = page, class = "fms-nav-item", `data-page` = page, icon(ico), tags$span(label), badge(count))
    drop <- function(label, ico, ...) {
      items <- Filter(Negate(is.null), list(...))
      if (!length(items)) return(NULL)
      total <- sum(vapply(items, function(x) x$count, 0))
      div(class = "dropdown fms-drop",
          tags$button(class = "fms-nav-item dropdown-toggle", type = "button", `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
                      icon(ico), tags$span(label), badge(total)),
          tags$ul(class = "dropdown-menu", lapply(items, function(x) tags$li(nav_link(page = x$page, class = "dropdown-item fms-nav-item", `data-page` = x$page,
                                                                                       icon(x$ico), tags$span(x$label), badge(x$count))))))
    }
    sub <- function(ok, page, label, ico, count = 0) if (ok) list(page = page, label = label, ico = ico, count = count)
    tagList(
      item("dashboard", "Dashboard", "table-cells-large"),
      drop("Landing Page", "rocket", sub(TRUE, "lp_allstar", "AllStar Vietnam Fund", "star"),
           sub(TRUE, "lp_commodity", "AllStar Commodity Fund", "star")),
      drop("Funds", "building-columns",
           sub(TRUE, "funds", "Fund list", "list"),
           sub(TRUE, "compare", "Compare funds", "code-compare"),
           sub(TRUE, "documents", "Documents library", "folder-open")),
      if (u$role %in% c("investor", "introducer")) item("room", room_label, if (u$role == "introducer") "users" else "wallet"), drop("Markets", "chart-line", sub(TRUE, "markets", "Markets", "chart-line"), sub(TRUE, "fx", "FX rates", "coins")),
      drop("Operations", "briefcase",
           sub(u$role %in% c("superadmin", "fund_manager"), "management", "Management", "briefcase"),
           sub(u$role %in% STAFF, "trading", "Trading", "right-left", n$working_trades),
           sub(u$role == "superadmin", "backoffice", "Back Office", "clipboard-check", n$pending_orders),
           sub(u$role %in% c("superadmin", "fund_manager"), "transactions", "Transactions", "money-bill-transfer"),
           sub(u$role %in% STAFF, "nav", "NAV & Pricing", "calculator"),
           sub(u$role %in% c("superadmin", "fund_manager"), "dividends", "Dividends", "hand-holding-dollar"),
           sub(u$role %in% c("superadmin", "fund_manager"), "commissions", "Commissions", "percent")),
      drop("Risk", "scale-balanced",
           sub(u$role %in% STAFF, "risk", "Risk & Exposure", "chart-pie"),
           sub(u$role %in% STAFF, "compliance", "Compliance", "list-check")),
      drop("Reports", "file-lines", sub(TRUE, "reports", "Reports", "file-arrow-down"), sub(TRUE, "calendar", "Calendar", "calendar-days")),
      drop("Help", "circle-question", sub(TRUE, "faq", "FAQ", "circle-question"), sub(TRUE, "glossary", "Glossary", "book"), sub(TRUE, "datastruct", "Data table structure", "table"),
           sub(TRUE, "helpdocs", "Documents", "file-lines"), sub(TRUE, "contact", "Contact", "envelope", n$open_requests)),
      drop("Admin", "shield-halved",
           sub(u$role == "superadmin", "supervision", "Supervision", "shield-halved"),
           sub(u$role == "superadmin", "users", "Users", "user-gear", n$pending_signups),
           sub(u$role == "superadmin", "audit", "Audit log", "clock-rotate-left")),
      tags$script(sprintf("FMS.setActive('%s')", isolate(st$page))))
  })

  outputOptions(output, "nav_menu", suspendWhenHidden = FALSE)  # it starts hidden behind the menu button on phones

  output$top_avatar <- renderUI({ u <- me(); req(u); avatar(u, 30) })
  observeEvent(input$act, if (input$act$action == "switch_role") {
    r <- input$act$id
    if (act(switch_role(me(), r), sprintf(tr("You are now working as %s."), tr(ROLES[[r]])))) go("dashboard")
  })
  output$account_menu <- renderUI({
    u <- me(); req(u)
    div(class = "fms-menu",
        div(class = "fms-menu-head", avatar(u, 44), div(tags$strong(u$display_name), tags$small(u$email), pill(ROLES[[u$role]], "gold"))),
        nav_link(page = "profile", class = "fms-menu-item", icon("user"), "My Profile"),
        if (u$role == "investor") nav_link(page = "myfunds", class = "fms-menu-item", icon("building-columns"), "My Funds"),
        if (u$role == "investor") nav_link(page = "myops", class = "fms-menu-item", icon("money-bill-transfer"), "My Operations"),
        if (u$role == "investor") nav_link(page = "mydivs", class = "fms-menu-item", icon("hand-holding-dollar"), "My Dividends"),
        nav_link(page = "room", class = "fms-menu-item", icon("wallet"), switch(u$role, investor = "My Portfolio", introducer = "My Clients", "My Activity")),
        # Users with several roles switch here; the pages and menus follow the role in use.
        if (length(roles_of(u)) > 1) tagList(tags$hr(class = "dropdown-divider"), tags$small(class = "fms-menu-label", "Switch role"),
          lapply(setdiff(roles_of(u), u$role), function(r) tags$a(href = "#", class = "fms-menu-item", `data-action` = "switch_role", `data-id` = r, icon("repeat"), ROLES[[r]]))),
        tags$hr(class = "dropdown-divider"),
        actionLink("logout", tagList(icon("right-from-bracket"), "Sign out"), class = "fms-menu-item fms-signout"))
  })

  notes <- reactive({ u <- me(); req(u); notifications(u) })
  output$bell_badge <- renderUI({ n <- length(notes()); if (n) tags$span(class = "fms-badge dot", n) })
  output$notif_list <- renderUI({
    items <- notes()
    if (!length(items)) return(tagList(div(class = "fms-pop-title", "Notifications"), div(class = "fms-empty", "You're all caught up.")))
    tagList(div(class = "fms-pop-title", "Notifications"), div(class = "fms-notes", lapply(head(items, 12), function(x)
      nav_link(page = x$link, class = "fms-note", tags$span(x$text), tags$small(fmt_date(x$at))))))
  })

  # These live in closed dropdowns, so keep them rendered in the background; they open instantly.
  for (id in c("top_avatar", "account_menu", "bell_badge", "notif_list")) outputOptions(output, id, suspendWhenHidden = FALSE)

  # Search: funds the user can see, plus users for the super administrator.
  observe(guard({
    st$user; st$lang; u <- isolate(me()); req(u)
    if (u$role == "superadmin") st$tick  # only the user list can change, so others rebuild on sign-in only
    f <- fund_rows(u)
    choices <- list(Funds = setNames(paste0("fund:", f$fund_code), paste(f$fund_code, "·", f$fund_name)))
    if (u$role == "superadmin") {
      us <- q("SELECT user_id, display_name, email FROM fms_users ORDER BY display_name")
      choices$Users <- setNames(paste0("user:", us$user_id), paste(us$display_name, "·", us$email))
    }
    updateSelectizeInput(session, "search", choices = choices, selected = character(0), server = FALSE,
                         options = list(placeholder = tr("Search funds or users")))
  }, "search list"))
  observeEvent(input$search, guard({
    v <- input$search
    if (startsWith(v, "fund:")) go("fund", sub("fund:", "", v), "overview")
    if (startsWith(v, "user:")) { st$edit_user <- as.integer(sub("user:", "", v)); go("users") }
    updateSelectizeInput(session, "search", selected = character(0))
  }, "search"), ignoreInit = TRUE)
}
