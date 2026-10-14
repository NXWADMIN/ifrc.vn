# Help pages: FAQ and Glossary. Content lives in R/help_data.R.

help_server <- function(input, output, session, st, go, me, act) {
  # Search box that hides the items whose text does not match (runs in the browser).
  help_search <- function(placeholder, empty) tagList(
    div(class = "fms-help-search", icon("magnifying-glass"),
        tags$input(type = "search", class = "form-control", placeholder = tr(placeholder),
                   oninput = "FMS.filterHelp(this)")),
    div(class = "fms-empty fms-help-none", style = "display:none", empty))

  output$pg_faq <- renderUI({
    req(me())
    tagList(
      page_head("FAQ", "Answers to common questions about FMS, funds, investing and your account."),
      help_search("Search the questions", "No question matches your search."),
      div(class = "fms-help-list", lapply(names(FAQ), function(g) div(class = "fms-help-group",
        tags$h2(class = "fms-help-title", g),
        lapply(FAQ[[g]], function(x) tags$details(class = "fms-help-item fms-faq",
          tags$summary(x[["q"]]), tags$p(x[["a"]])))))))
  })

  output$pg_glossary <- renderUI({
    req(me())
    terms <- vapply(GLOSSARY, function(x) tr(x[["term"]]), "")
    # Sort by the translated term, with the alphabet of the current language (Vietnamese accents, Chinese pinyin).
    ord <- if (requireNamespace("stringi", quietly = TRUE))
      stringi::stri_order(terms, opts_collator = stringi::stri_opts_collator(locale = current_lang())) else order(tolower(terms))
    items <- GLOSSARY[ord]
    tagList(
      page_head("Glossary", "Key fund and investment terms used across FMS."),
      help_search("Search the glossary", "No term matches your search."),
      div(class = "fms-help-list fms-gloss", lapply(items, function(x) div(class = "fms-help-item",
        tags$dt(x[["term"]]), tags$dd(x[["def"]])))))
  })

  # ---------- Data table structure: the database tables, then the fields of the one clicked
  ds_sel <- reactiveVal(names(DATA_TABLES)[1])
  observeEvent(input$act, if (input$act$action == "ds_table" && input$act$id %in% names(DATA_TABLES)) ds_sel(input$act$id))

  output$pg_datastruct <- renderUI({
    req(me())
    tagList(
      page_head("Data table structure", "The tables in the FMS database. Click a table name to see its fields below."),
      fms_card("Tables", uiOutput("ds_tables")),
      fms_card(uiOutput("ds_title", inline = TRUE), uiOutput("ds_fields")))
  })
  output$ds_tables <- renderUI({
    t <- data_tables(); sel <- ds_sel()
    html_table(t, list(
      Name = function(r) tags$a(href = "#", class = "fms-ds-link", `data-action` = "ds_table", `data-id` = r$table, r$table),
      Description = function(r) tr(r$description),
      `#Fields` = function(r) r$fields,
      `#Rows` = function(r) fmt_num(r$rows, 0)),
      row_class = function(r) if (r$table == sel) "fms-row-sel")
  })
  output$ds_title <- renderUI(tagList(sprintf(tr("Structure of %s"), ds_sel()),
                                      tags$small(sprintf(tr("%d fields"), nrow(data_fields(ds_sel()))))))
  output$ds_fields <- renderUI({
    u <- me(); req(u); t <- ds_sel(); f <- data_fields(t)
    # Never show secrets, and only the super administrator sees example personal data.
    hide <- data_hidden(t, f$field, u)
    f$example[hide & f$example != ""] <- "\u2022\u2022\u2022"
    html_table(f, list(
      `#` = function(r) r$n,
      Field = function(r) tags$code(class = "fms-field", translate = "no", r$field),
      Type = function(r) tags$span(class = paste("fms-type", r$type), translate = "no", r$type),
      Example = function(r) if (r$example == "") tags$span(class = "muted", "\u2014") else tags$span(translate = "no", r$example)),
      source = t)
  })

  # ---------- Documents: the help documents, each as a page to view or an HTML file to download
  doc_url <- session$registerDataObj("fms_helpdoc", list(), function(data, req) {
    id <- shiny::parseQueryString(req$QUERY_STRING)$d
    shiny::withReactiveDomain(session, isolate(tryCatch({
      if (is.null(me())) stop("Please sign in again.")
      html_resp(help_doc_html(id, me()))
    }, error = function(e) html_resp(view_page(tr("Document unavailable"), "", paste0("<p>", htmltools::htmlEscape(tr(conditionMessage(e))), "</p>")), 404L))))
  })
  for (id in names(HELP_DOCS)) local({
    d <- id
    output[[paste0("hd_", d)]] <- downloadHandler(filename = function() sprintf("fms-%s-%s.html", d, format(Sys.time(), "%Y%m%d", tz = "Asia/Ho_Chi_Minh")),
                                                  content = function(file) writeLines(help_doc_html(d, me()), file, useBytes = TRUE))
  })
  output$pg_helpdocs <- renderUI({
    req(me())
    tagList(
      page_head("Documents", "Guides and references for FMS. View them in a new tab or download them to read offline and print."),
      div(class = "fms-doc-grid", lapply(names(HELP_DOCS), function(id) { d <- HELP_DOCS[[id]]
        div(class = "fms-card fms-doc-card",
            div(class = "fms-doc-icon", icon(d$icon)),
            tags$h3(d$title), tags$p(class = "muted", d$about),
            div(class = "fms-doc-actions",
                tags$a(class = "btn btn-outline-primary fms-view-btn", href = paste0(doc_url, "&d=", id), target = "_blank", rel = "noopener", icon("up-right-from-square"), "View"),
                downloadButton(paste0("hd_", id), "Download", class = "btn-primary"))) })))
  })

  # ---------- Contact: the contact details and the request information form
  output$pg_contact <- renderUI({
    u <- me(); req(u)
    funds <- fund_rows(u)
    tagList(
      page_head("Contact", "Write to us or send a request for information. We answer by e-mail or phone."),
      layout_columns(col_widths = c(4, 8),
        fms_card("Contact us",
          div(class = "fms-contact",
              div(icon("building"), div(tags$small("Company"), tags$b(translate = "no", CONTACT$company))),
              div(icon("envelope"), div(tags$small("E-mail"), tags$a(translate = "no", href = paste0("mailto:", CONTACT$email), CONTACT$email))),
              div(icon("location-dot"), div(tags$small("Address"), tags$b(CONTACT$address))),
              div(icon("clock"), div(tags$small("Office hours"), tags$b(CONTACT$hours))))),
        fms_card("Request information",
          div(class = "fms-form-grid two",
              selectInput("rq_topic", "Topic", setNames(REQUEST_TOPICS, vapply(REQUEST_TOPICS, tr, "")), width = "100%"),
              selectInput("rq_fund", "Fund", c(setNames("", tr("Not about a specific fund")), setNames(funds$fund_code, paste(funds$fund_code, "\u00b7", funds$fund_name))), width = "100%")),
          textInput("rq_subject", "Subject", width = "100%"),
          textAreaInput("rq_message", "Message", width = "100%", rows = 5, resize = "vertical"),
          div(class = "fms-rq-foot",
              radioButtons("rq_reply", "Reply by", c(`E-mail` = "email", Phone = "phone"), inline = TRUE),
              tags$small(class = "muted", sprintf(tr("Sent as %s (%s)"), u$display_name, u$email)),
              actionButton("rq_send", "Send request", class = "btn-primary", icon = icon("paper-plane"))))),
      uiOutput("rq_list"))
  })
  observeEvent(input$rq_send, {
    ok <- act(send_request(me(), input$rq_topic, input$rq_fund, input$rq_subject, input$rq_message, input$rq_reply),
              "Your request has been sent. We will get back to you soon.")
    if (ok) { updateTextInput(session, "rq_subject", value = ""); updateTextAreaInput(session, "rq_message", value = "") }
  })
  observeEvent(input$act, if (input$act$action == "rq_answered") act(answer_request(me(), as.integer(input$act$id)), "Request marked as answered."))
  output$rq_list <- renderUI({
    u <- me(); req(u); st$tick
    admin <- u$role == "superadmin"
    sql <- "SELECT r.*, u.display_name, u.email, u.phone, f.fund_code FROM fms_requests r JOIN fms_users u USING (user_id) LEFT JOIN fms_fund_list f USING (fund_id)"
    r <- if (admin) q(paste(sql, "ORDER BY r.created_at DESC")) else q(paste(sql, "WHERE r.user_id = ? ORDER BY r.created_at DESC"), u$user_id)
    cols <- list(Sent = function(r) tags$span(class = "text-nowrap", sub("T", " ", substr(r$created_at, 1, 16))),
                 From = function(r) div(tags$b(r$display_name), tags$small(class = "muted d-block", if (r$reply_by == "phone") nz(r$phone, r$email) else r$email)),
                 Topic = function(r) tr(r$topic), Fund = function(r) nz(r$fund_code, "\u2014"),
                 Request = function(r) div(class = "fms-rq-text", tags$b(r$subject), tags$p(r$message)),
                 Reply = function(r) if (r$reply_by == "phone") "Phone" else "E-mail",
                 Status = function(r) if (r$status == "open") pill("Open", "warn") else pill("Answered", "good"),
                 ` ` = function(r) if (r$status == "open") tags$button(class = "btn btn-sm btn-outline-primary", `data-action` = "rq_answered", `data-id` = r$request_id, "Mark answered"))
    if (!admin) cols$From <- cols$` ` <- NULL
    fms_card(tagList(if (admin) "All requests" else "My requests", tags$small(sprintf(tr("%d sent"), nrow(r)))),
             html_table(r, cols, empty = "No requests yet.", source = "fms_requests"))
  })
}

# One row per table: name, description, number of fields and of rows, read from the live database.
data_tables <- function() {
  t <- names(DATA_TABLES)
  data.frame(table = t, description = unname(DATA_TABLES),
             fields = vapply(t, function(x) length(DBI::dbListFields(con, x)), 0L),
             rows = vapply(t, function(x) as.numeric(q(sprintf("SELECT COUNT(*) AS n FROM %s", x))$n), 0),
             stringsAsFactors = FALSE)
}

# The fields of one table with their type (as the R data files hold them) and the first non-empty value as an example.
data_fields <- function(table) cached(paste0("ds_", table), {
  stopifnot(table %in% names(DATA_TABLES))
  info <- q(sprintf("PRAGMA table_info(%s)", table))
  type <- ifelse(grepl("(_date|^date_of_birth)$", info$name), "Date",
          ifelse(grepl("(_at|^ts)$", info$name), "POSIXct",
          ifelse(grepl("INT", toupper(info$type)), "integer",
          ifelse(grepl("REAL|NUM|DOUB|FLOA", toupper(info$type)), "numeric", "character"))))
  # Examples come from the first row, so they belong together; an empty field takes its first filled value instead.
  first <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 1", table))
  ex <- vapply(info$name, function(n) {
    v <- if (nrow(first)) first[[n]][1] else NA
    if (is.null(v) || is.na(v) || identical(format(v), ""))
      v <- DBI::dbGetQuery(con, sprintf('SELECT "%s" AS v FROM %s WHERE "%s" IS NOT NULL AND "%s" <> \'\' LIMIT 1', n, table, n, n))$v
    if (!length(v)) return("")
    v <- format(v[1]); if (nchar(v) > 40) paste0(substr(v, 1, 39), "\u2026") else v
  }, "")
  data.frame(n = seq_len(nrow(info)), field = info$name, type = type, example = unname(ex), stringsAsFactors = FALSE)
})

# Field examples that are not shown: secrets always, other people's personal data unless super administrator.
data_hidden <- function(table, fields, u) {
  admin <- u$role == "superadmin"
  fields %in% c("password_hash", "token") |
    (table == "fms_users" & !admin & !fields %in% c("user_id", "role", "roles", "status", "fund_access")) |
    (table == "fms_requests" & !admin & fields %in% c("subject", "message"))
}

# ---------- Help documents
HELP_DOCS <- list(
  guide = list(title = "User guide", icon = "book-open", about = "How FMS is organised and what each part of the menu does."),
  faq = list(title = "FAQ", icon = "circle-question", about = "Every frequently asked question with its answer, on one page."),
  glossary = list(title = "Glossary", icon = "book", about = "Key fund and investment terms used across FMS."),
  datadict = list(title = "Data dictionary", icon = "table", about = "Every database table with its fields, types and an example."))

help_doc_html <- function(id, u) {
  d <- HELP_DOCS[[id]]; if (is.null(d)) stop("Unknown document.")
  e <- function(v) htmltools::htmlEscape(tr(as.character(v)))
  body <- switch(id,
    guide = paste0(vapply(USER_GUIDE, function(x) paste0("<h2>", e(x[["h"]]), "</h2><p>", e(x[["p"]]), "</p>"), ""), collapse = ""),
    faq = paste0(vapply(names(FAQ), function(g) paste0("<h2>", e(g), "</h2>", paste0(vapply(FAQ[[g]], function(x) paste0("<h3>", e(x[["q"]]), "</h3><p>", e(x[["a"]]), "</p>"), ""), collapse = "")), ""), collapse = ""),
    glossary = {
      terms <- vapply(GLOSSARY, function(x) tr(x[["term"]]), "")
      ord <- if (requireNamespace("stringi", quietly = TRUE)) stringi::stri_order(terms, opts_collator = stringi::stri_opts_collator(locale = current_lang())) else order(tolower(terms))
      paste0("<dl>", paste0(vapply(GLOSSARY[ord], function(x) paste0("<dt>", e(x[["term"]]), "</dt><dd>", e(x[["def"]]), "</dd>"), ""), collapse = ""), "</dl>")
    },
    datadict = paste0(vapply(names(DATA_TABLES), function(t) {
      f <- data_fields(t); f$example[data_hidden(t, f$field, u) & f$example != ""] <- "\u2022\u2022\u2022"
      names(f) <- c("#", tr("Field"), tr("Type"), tr("Example"))
      paste0("<h2>", htmltools::htmlEscape(t), "</h2><p>", e(DATA_TABLES[[t]]), "</p>", view_table(f))
    }, ""), collapse = ""))
  sub <- sprintf(tr("Generated %s"), format(Sys.time(), "%Y-%m-%d %H:%M", tz = "Asia/Ho_Chi_Minh"))
  view_page(tr(d$title), sub, paste0("<div class='doc'>", body, "</div>"))
}

# ---------- Requests for information
send_request <- function(u, topic, fund, subject, message, reply_by) {
  subject <- trimws(nz(subject, "")); message <- trimws(nz(message, ""))
  if (!topic %in% REQUEST_TOPICS) fail("Please choose a topic.")
  if (nchar(subject) < 3) fail("Please enter a subject.")
  if (nchar(subject) > 150) fail("The subject is too long (150 characters at most).")
  if (nchar(message) < 10) fail("Please write your request (10 characters at least).")
  if (nchar(message) > 3000) fail("The message is too long (3,000 characters at most).")
  fund_id <- NA
  if (nzchar(nz(fund, ""))) { f <- fund_rows(u); f <- f[f$fund_code == fund, ]; if (!nrow(f)) fail("Please choose a fund from the list."); fund_id <- f$fund_id[1] }
  run("INSERT INTO fms_requests (user_id, topic, fund_id, subject, message, reply_by, status, created_at) VALUES (?, ?, ?, ?, ?, ?, 'open', ?)",
      u$user_id, topic, fund_id, subject, message, if (identical(reply_by, "phone")) "phone" else "email", now_iso())
  audit(u$user_id, "request.send", sprintf("Request: %s", subject))
}

answer_request <- function(u, id) {
  if (u$role != "superadmin") fail("Your role does not have access to this area.")
  r <- q1("SELECT * FROM fms_requests WHERE request_id = ?", id)
  if (is.null(r) || r$status != "open") fail("This request is already answered.")
  run("UPDATE fms_requests SET status = 'answered', answered_by = ?, answered_at = ? WHERE request_id = ?", u$user_id, now_iso(), id)
  audit(u$user_id, "request.answer", sprintf("Answered request %d: %s", id, r$subject))
}
