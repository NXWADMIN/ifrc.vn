# Number formats and small HTML building blocks shared by every page.

# Unexpected errors are written to the server log (rsconnect::showLogs) and shown as a message,
# instead of ending the user's session with "Disconnected from the server".
report_error <- function(e, where = "action") {
  message(sprintf("[FMS] error in %s: %s", where, conditionMessage(e)))
  try(showNotification(paste(tr("Something went wrong:"), conditionMessage(e)), type = "error", duration = 12), silent = TRUE)
  invisible(NULL)
}
guard <- function(expr, where = "action") {
  tryCatch(expr, shiny.silent.error = function(e) invisible(NULL),
           fms_error = function(e) showNotification(conditionMessage(e), type = "error"),
           error = function(e) report_error(e, where))
}

fmt_num <- function(x, digits = 2) ifelse(is.na(x), "–", formatC(x, format = "f", digits = digits, big.mark = ","))
fmt_pct <- function(x, digits = 2, sign = TRUE) {
  ifelse(is.na(x), "–", paste0(ifelse(sign & x > 0, "+", ""), formatC(x * 100, format = "f", digits = digits), "%"))
}
fmt_money <- function(x, ccy) fmt_num(x, ifelse(ccy == "VND", 0, 2))
fmt_nav <- function(x, ccy) fmt_num(x, ifelse(ccy == "VND", 0, 4))
fmt_compact <- function(x, prefix = "") {
  vapply(x, function(v) {
    if (is.na(v)) return("–")
    a <- abs(v)
    s <- if (a >= 1e12) paste0(formatC(v / 1e12, format = "f", digits = 2), "T")
         else if (a >= 1e9) paste0(formatC(v / 1e9, format = "f", digits = 2), "B")
         else if (a >= 1e6) paste0(formatC(v / 1e6, format = "f", digits = 1), "M")
         else if (a >= 1e3) paste0(formatC(v / 1e3, format = "f", digits = 1), "K")
         else formatC(v, format = "f", digits = 0)
    paste0(prefix, s)
  }, "")
}
fmt_date <- function(d) {
  out <- rep("–", length(d)); ok <- !is.na(d) & d != ""
  if (any(ok)) out[ok] <- local_date(as.Date(substr(d[ok], 1, 10)))
  out
}
fmt_datetime <- function(ts) {
  out <- rep("–", length(ts)); ok <- !is.na(ts)
  if (any(ok)) { t <- as.POSIXct(ts[ok], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"); out[ok] <- local_date(as.Date(t), format(t, "%H:%M")) }
  out
}
cls <- function(x) ifelse(is.na(x), "", ifelse(x > 0, "up", ifelse(x < 0, "down", "")))

pct_cell <- function(x, digits = 2) tags$span(class = paste("num", cls(x)), fmt_pct(x, digits))
pill <- function(text, kind = "") tags$span(class = paste("fms-pill", kind), text)
status_pill <- function(status) {
  kind <- c(pending = "warn", active = "good", approved = "good", settled = "good", filled = "good", paid = "good", open = "good",
            working = "info", declared = "info", accrued = "info", rejected = "bad", cancelled = "muted", suspended = "bad",
            breach = "bad", watch = "warn", ok = "good")
  pill(tools::toTitleCase(gsub("_", " ", status)), nz(unname(kind[status]), ""))
}

initials <- function(name) {
  w <- strsplit(trimws(name), "\\s+")[[1]]
  toupper(paste0(substr(w[1], 1, 1), if (length(w) > 1) substr(w[length(w)], 1, 1) else ""))
}
avatar <- function(user, size = 36) {
  style <- sprintf("width:%dpx;height:%dpx;font-size:%dpx", size, size, round(size * 0.38))
  if (!is.null(user$avatar) && !is.na(user$avatar) && nzchar(user$avatar)) tags$img(class = "fms-avatar", src = user$avatar, style = style, alt = "")
  else tags$span(class = "fms-avatar", style = style, initials(user$display_name))
}

# A link that switches page through the client-side router in www/fms.js.
nav_link <- function(..., page, fund = NULL, tab = NULL, class = NULL) {
  tags$a(href = "#", class = class, `data-nav` = page, `data-fund` = fund, `data-tab` = tab, ...)
}
fund_link <- function(code, name = code, tab = "overview") nav_link(page = "fund", fund = code, tab = tab, class = "fms-fund-link", name)

# Data table: cols is a named list of functions(row) -> cell content; names are the headers.
# A header starting with "#" is right-aligned (numbers).
# The RDS file(s) a table reads from, shown under its bottom-right corner so it is quick to find where to update the data.
rds_source <- function(...) {
  files <- c(...); if (!length(files)) return(NULL)
  div(class = "fms-src", title = "Data source (data/rds)", icon("database"), paste0(files, ".rds", collapse = " · "))
}

html_table <- function(df, cols, class = "fms-table", empty = "Nothing to show yet.", row_class = NULL, source = NULL) {
  if (is.null(df) || !nrow(df)) return(tagList(div(class = "fms-empty", empty), rds_source(source)))
  heads <- names(cols)
  num <- startsWith(heads, "#")
  # Tables that start with a name (fund, client, instrument...) give that column 30% of the width.
  if (heads[1] %in% c("Fund", "Name", "Client", "Instrument", "Position", "Sleeve", "Contract")) class <- paste(class, "fms-name-first")
  tags$div(class = "fms-table-wrap", tags$table(class = class,
    tags$thead(tags$tr(lapply(seq_along(heads), function(i) tags$th(class = if (num[i]) "num", sub("^#", "", heads[i]))))),
    tags$tbody(lapply(seq_len(nrow(df)), function(r) {
      row <- df[r, , drop = FALSE]
      tags$tr(class = if (!is.null(row_class)) row_class(row),
              lapply(seq_along(cols), function(i) tags$td(class = if (num[i]) "num", cols[[i]](row))))
    }))), rds_source(source))
}

stat_tile <- function(label, value, sub = NULL, value_class = NULL) {
  div(class = "fms-stat", tags$span(class = "fms-stat-label", label), tags$strong(class = value_class, value),
      if (!is.null(sub)) tags$small(sub))
}

page_head <- function(title, subtitle = NULL, ...) {
  div(class = "fms-page-head", div(div(class = "fms-app-eyebrow", "Your funds in one place"),
                                          div(class = "fms-app-title", tags$span("Funds"), tags$em("Management System"), tags$small("(FMS)")), tags$h1(title), if (!is.null(subtitle)) tags$p(subtitle)), div(class = "fms-page-actions", ...))
}

# Section title inside a card: gold icon chip, title and an optional note.
fms_section <- function(title, ico, note = NULL) {
  div(class = "fms-section-title", tags$span(class = "fms-section-ico", icon(ico)), tags$h3(title), if (!is.null(note)) tags$small(note))
}

fms_card <- function(title = NULL, ..., class = NULL, actions = NULL) {
  div(class = paste("fms-card", class),
      if (!is.null(title)) div(class = "fms-card-head", tags$h3(title), actions),
      ...)
}

# Dark-mode friendly plotly layout.
fms_plot_layout <- function(p, yfmt = NULL, ysuffix = "", legend = TRUE, hovermode = "x unified") {
  axis <- list(gridcolor = "rgba(140,160,190,0.15)", zeroline = FALSE, linecolor = "rgba(140,160,190,0.3)",
               tickfont = list(color = "#8b98ae", size = 11), title = "")
  plotly::layout(p, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", font = list(family = "Inter, sans-serif", color = "#8b98ae"),
                 xaxis = axis, yaxis = modifyList(axis, list(tickformat = yfmt, ticksuffix = ysuffix)), hovermode = hovermode,
                 showlegend = legend, legend = list(orientation = "h", x = 0, y = 1.12, font = list(color = "#8b98ae")),
                 margin = list(l = 50, r = 16, t = 30, b = 36), hoverlabel = list(font = list(family = "Inter, sans-serif"))) |>
    plotly::config(displayModeBar = FALSE)
}
SERIES <- c("#4f8bff", "#e3b453", "#22c38e", "#b07cff", "#f2566b", "#4fb4ff")

# Native browser date field; its value is "YYYY-MM-DD" or NULL when empty (binding in www/fms.js).
date_field <- function(id, label, value = NA) {
  div(class = "form-group shiny-input-container", tags$label(class = "control-label", `for` = id, label),
      tags$input(id = id, type = "date", class = "form-control fms-date", value = if (is.na(value)) NULL else substr(value, 1, 10)))
}

# View pages: a report or a help document as a web page in a new browser tab, with a Print button.
view_page <- function(title, sub, body) {
  e <- function(v) htmltools::htmlEscape(as.character(v))
  paste0("<!doctype html><html lang='", current_lang(), "'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'><title>FMS · ", e(title), "</title><style>",
    "body{font-family:Inter,Arial,sans-serif;color:#0d1a33;background:#f4f6fb;margin:0;padding:24px}main{max-width:1280px;margin:0 auto;background:#fff;border-radius:14px;padding:24px;box-shadow:0 4px 20px rgba(13,26,51,.08)}",
    ".top{display:flex;justify-content:space-between;align-items:flex-start;gap:16px}.brand{color:#9a6a0f;font-weight:800;letter-spacing:.15em;font-size:13px}h1{color:#0a6cc0;font-size:22px;margin:4px 0}.muted{color:#55648a;font-size:13px}",
    "button{background:linear-gradient(90deg,#f0c766,#c9962e);border:0;border-radius:8px;padding:8px 16px;font-weight:600;cursor:pointer}",
    ".wrap{overflow-x:auto;margin-top:16px}table{width:100%;border-collapse:collapse;font-size:13px}th{background:#1a4299;color:#f3c86a;text-align:left;padding:8px;white-space:nowrap;text-transform:uppercase;font-size:11px;letter-spacing:.04em;position:sticky;top:0}",
    "td{padding:6px 8px;border-bottom:1px solid #e3e8f2;white-space:nowrap}tr:nth-child(even) td{background:#f8faff}.n{text-align:right;font-variant-numeric:tabular-nums}",
    ".doc{max-width:860px;font-size:14px;line-height:1.6}.doc h2{font-size:14px;text-transform:uppercase;letter-spacing:.06em;color:#9a6a0f;border-bottom:2px solid #e3b453;padding-bottom:4px;margin:26px 0 10px}.doc h3{font-size:15px;margin:16px 0 4px;color:#0d1a33}.doc p{margin:0 0 10px;color:#33415e}.doc dt{font-weight:700;margin-top:12px;color:#0a6cc0}.doc dd{margin:2px 0 0;color:#33415e}.doc code{font-size:12px}",
    "@media print{body{background:#fff;padding:0}main{box-shadow:none;padding:0}button{display:none}}</style></head><body><main>",
    "<div class='top'><div><div class='brand'>FMS</div><h1>", e(title), "</h1><div class='muted'>", e(sub), "</div></div><button onclick='window.print()'>", e(tr("Print")), "</button></div>",
    body, "</main></body></html>")
}
view_table <- function(df) {
  e <- function(v) htmltools::htmlEscape(as.character(v))
  if (!nrow(df)) return(paste0("<p class='muted'>", e(tr("No data.")), "</p>"))
  num <- vapply(df, is.numeric, logical(1))
  cells <- lapply(names(df), function(k) { v <- df[[k]]
    if (is.numeric(v)) ifelse(is.na(v), "", formatC(v, format = "f", digits = if (all(is.na(v) | v == round(v))) 0 else 4, big.mark = ",", drop0trailing = TRUE))
    else ifelse(is.na(v), "", e(v)) })
  rows <- do.call(paste0, lapply(seq_along(cells), function(j) paste0("<td", if (num[j]) " class='n'" else "", ">", cells[[j]], "</td>")))
  paste0("<div class='wrap'><table><thead><tr>", paste0("<th", ifelse(num, " class='n'", ""), ">", e(gsub("_", " ", names(df))), "</th>", collapse = ""),
         "</tr></thead><tbody>", paste0("<tr>", rows, "</tr>", collapse = ""), "</tbody></table></div>")
}
html_resp <- function(html, status = 200L) shiny::httpResponse(status, "text/html; charset=utf-8", enc2utf8(html))
