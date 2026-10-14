# FMS, Funds Management System: R Shiny version.
# Run from this folder with  R -e 'shiny::runApp(port = 3838)'  (files in R/ load automatically).
library(shiny)
library(bslib)

fms_theme <- bs_theme(
  version = 5, primary = "#c9962e", secondary = "#2f6bff",
  base_font = font_google("Inter", local = FALSE), font_scale = 0.92,
  "body-bg-dark" = "#060c19", "body-color-dark" = "#dfe6f3", "body-tertiary-bg-dark" = "#0b1426",
  "body-secondary-bg-dark" = "#0e1a31", "border-color-dark" = "#1a2944", "link-color-dark" = "#e3b453",
  "body-bg" = "#f4f6fb", "body-color" = "#14213d", "border-color" = "#dfe4ee", "link-color" = "#a87a12",
  "border-radius" = "10px"
)

PAGES <- c("dashboard", "funds", "fund", "room", "profile", "markets", "management", "trading", "backoffice", "supervision", "users", "transactions", "nav", "risk", "compare", "documents", "dividends", "commissions", "compliance", "audit", "fx", "reports", "calendar", "myfunds", "myops", "mydivs", "faq", "glossary", "datastruct", "helpdocs", "contact", "lp_allstar", "lp_commodity")

topbar <- tags$header(class = "fms-header",
  tags$a(class = "fms-brand", href = "#", `data-nav` = "dashboard", title = "FMS",
         tags$img(class = "fms-nxw-logo fms-head-logo", src = "nxw-logo.png", alt = "NXW Next Wealth")),
  tags$button(class = "fms-icon-btn fms-burger", type = "button", title = "Menu", onclick = "document.body.classList.toggle('fms-menu-open')", icon("bars")),
  tags$nav(class = "fms-hnav", `aria-label` = "Main menu", uiOutput("nav_menu", inline = TRUE)),
  div(class = "fms-top-icons",
    div(class = "fms-search", selectizeInput("search", NULL, choices = NULL, width = "100%",
                                             options = list(placeholder = "Search funds or users"))),
    # Bootstrap dropdowns anchored to the right edge, so the menus open under their button and never cover the page title.
    div(class = "dropdown fms-top-drop",
        tags$button(class = "fms-icon-btn", type = "button", title = "Notifications", `data-bs-toggle` = "dropdown", `data-bs-offset` = "0,10",
                    `aria-expanded` = "false", icon("bell"), uiOutput("bell_badge", inline = TRUE)),
        div(class = "dropdown-menu dropdown-menu-end fms-pop", uiOutput("notif_list"))),
    div(class = "dropdown fms-top-drop",
        tags$button(class = "fms-icon-btn", type = "button", title = "Options", `data-bs-toggle` = "dropdown", `data-bs-offset` = "0,10",
                    `aria-expanded` = "false", `data-bs-auto-close` = "outside", icon("sliders")),
        div(class = "dropdown-menu dropdown-menu-end fms-pop", uiOutput("options_menu"))),
    div(class = "dropdown fms-top-drop",
        tags$button(class = "fms-avatar-btn", type = "button", title = "Account", `data-bs-toggle` = "dropdown", `data-bs-offset` = "0,10",
                    `aria-expanded` = "false", uiOutput("top_avatar", inline = TRUE)),
        div(class = "dropdown-menu dropdown-menu-end fms-pop", uiOutput("account_menu")))))

# The sign-in screen is rendered by the server (R/shell.R) so it follows the chosen language.
# Behind it: the starfield photo drifting slowly, with twinkling stars drawn on a canvas (www/fms.js).
ASSET_V <- format(max(file.mtime(c("www/fms.css", "www/fms.js"))), "%Y%m%d%H%M%S")

auth_overlay <- conditionalPanel("!output.signed_in", class = "fms-auth",
  div(class = "fms-space", `aria-hidden` = "true"), tags$canvas(id = "fms-stars", `aria-hidden` = "true"), div(class = "fms-vignette"),
  uiOutput("auth_ui"))

# Loading popup shown from the first paint until the server is connected and the first screen is drawn (www/fms.js).
loader <- div(id = "fms-loader", class = "fms-loader", role = "status", `aria-live` = "polite",
  div(class = "fms-loader-card",
    tags$img(class = "fms-loader-logo", src = "nxw-logo.png", alt = "NXW Next Wealth"),
    div(class = "fms-app-title fms-loader-title", tags$span("Funds"), tags$em("Management System"), tags$small("(FMS)")),
    div(class = "fms-loader-spin", `aria-hidden` = "true"),
    div(class = "fms-loader-text", "Preparing your funds..."),
    div(class = "fms-loader-bar", div())))

ui <- page(
  theme = fms_theme, title = "Funds Management System (FMS)",
  tags$head(
    # Apply the saved mode before the page paints, so it never flashes the other colours.
    tags$script(HTML("try{document.documentElement.setAttribute('data-bs-theme',localStorage.getItem('fms-theme')||'dark')}catch(e){document.documentElement.setAttribute('data-bs-theme','dark')}")),
    # A saved sign-in: hide the sign-in card until the server has signed the user back in (no flash of the form).
    tags$script(HTML("try{if(localStorage.getItem('fms-token')||sessionStorage.getItem('fms-token'))document.documentElement.classList.add('fms-resuming')}catch(e){}")),
    # Lora serif for the app title (gold italic half), as on the Indexes Observatory.
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Lora:ital,wght@0,400;0,700;1,500&display=swap"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "fms-mark.svg"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32.png"),
    tags$link(rel = "apple-touch-icon", href = "apple-touch-icon.png"),
    # The version stamp makes browsers fetch the new files after each deploy instead of a cached copy.
    tags$link(rel = "stylesheet", href = paste0("fms.css?v=", ASSET_V)), tags$script(src = paste0("fms.js?v=", ASSET_V))),
  loader,
  auth_overlay,
  topbar,
  tags$main(class = "fms-main",
    do.call(navset_hidden, c(list(id = "page"), lapply(PAGES, function(p) nav_panel_hidden(p, uiOutput(paste0("pg_", p)))))))
)

server <- function(input, output, session) {
  st <- reactiveValues(user = NULL, page = "dashboard", fund = NULL, tab = "overview", tick = 0, lang = "en")
  session$userData$lang <- reactive(st$lang)
  bump <- function() st$tick <- isolate(st$tick) + 1
  me <- reactive({ st$tick; if (is.null(st$user)) NULL else user_by_id(st$user$user_id) })

  # Runs a user action; shows its error as a notification, or ok as a confirmation, and refreshes pages.
  act <- function(expr, ok = NULL) {
    tryCatch({
      force(expr)
      if (!is.null(ok)) showNotification(ok, type = "message")
      bump(); TRUE
    }, shiny.silent.error = function(e) FALSE,
       fms_error = function(e) { showNotification(conditionMessage(e), type = "error"); FALSE },
       error = function(e) { report_error(e); FALSE })
  }

  output$signed_in <- reactive(!is.null(st$user))
  outputOptions(output, "signed_in", suspendWhenHidden = FALSE)

  go <- function(page, fund = NULL, tab = NULL) {
    if (!page %in% PAGES) page <- "dashboard"
    if (!is.null(fund)) st$fund <- fund
    if (!is.null(tab)) st$tab <- tab
    st$page <- page
    nav_select("page", page)
    if (page == "fund" && !is.null(tab)) nav_select("fund_tab", tab)
    session$sendCustomMessage("fms-active", page)
  }
  observeEvent(input$nav, guard(go(input$nav$page, nz(input$nav$fund, NULL), nz(input$nav$tab, NULL)), "navigation"))

  options_server(input, output, session, st, me)
  auth_server(input, output, session, st, go, me)
  shell_server(input, output, session, st, go, me, act)
  funds_server(input, output, session, st, go, me, act)
  account_server(input, output, session, st, go, me, act)
  ops_server(input, output, session, st, go, me, act)
  more_server(input, output, session, st, go, me, act)
  help_server(input, output, session, st, go, me, act)
  landing_server(input, output, session, st, go, me, act)
}

# shinyApp(ui, server)

# Bind to 0.0.0.0 and listen on Render's PORT
shiny::runApp(
  shinyApp(ui, server),
  host = "0.0.0.0",
  port = as.numeric(Sys.getenv("PORT", "3838"))
)