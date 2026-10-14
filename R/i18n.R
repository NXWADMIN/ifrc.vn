# Interface languages: English (default), Vietnamese, French and Chinese.
# English text is the key; i18n/vi.json, i18n/fr.json and i18n/zh.json map it to the translation.
# Every renderUI() runs its result through tr_tags(), which translates each text node,
# tooltip and placeholder that matches a key. Sentences built with sprintf() or paste()
# translate their template with tr(). Values sent to the server (ids, codes) never change.

LANGS <- c(en = "English", vi = "Tiếng Việt", fr = "Français", zh = "中文")
THEMES <- c("dark", "light")

.i18n <- local({
  d <- list()
  for (l in setdiff(names(LANGS), "en")) {
    f <- file.path("i18n", paste0(l, ".json"))
    if (file.exists(f)) { v <- jsonlite::read_json(f); d[[l]] <- unlist(v[lengths(v) == 1]) }
  }
  d
})

# The session's language. Reading it inside a render makes that render follow language changes.
current_lang <- function() {
  s <- shiny::getDefaultReactiveDomain()
  if (is.null(s) || is.null(s$userData$lang)) return("en")
  l <- s$userData$lang()
  if (is.null(l) || !l %in% names(LANGS)) "en" else l
}

tr <- function(s) {
  if (!is.character(s) || !length(s)) return(s)
  lang <- current_lang()
  if (lang == "en" || is.null(.i18n[[lang]])) return(s)
  v <- .i18n[[lang]][s]
  unname(ifelse(is.na(v), s, v))
}

tr_tags <- function(x) {
  lang <- current_lang()
  if (lang == "en" || is.null(.i18n[[lang]])) return(x)
  .tr_walk(x, .i18n[[lang]])
}
.tr_walk <- function(x, d) {
  if (is.null(x)) return(x)
  if (inherits(x, "html")) {
    # Choice lists (select, radio, checkbox) arrive as HTML strings: translate the text between tags.
    if (length(x) == 1 && !grepl("<", x)) { v <- d[trimws(x)]; return(if (is.na(v)) x else HTML(unname(v))) }
    at <- attributes(x)
    m <- gregexpr(">[^<>]+<", x)
    regmatches(x, m) <- lapply(regmatches(x, m), function(t) {
      inner <- substr(t, 2, nchar(t) - 1); key <- trimws(inner); v <- d[key]
      if (!length(t)) t else ifelse(is.na(v), t, paste0(">", unname(v), "<"))
    })
    attributes(x) <- at
    return(x)
  }
  if (is.character(x)) {
    if (!length(x)) return(x)
    v <- d[x]
    return(unname(ifelse(is.na(v), x, v)))
  }
  if (inherits(x, "shiny.tag")) {
    if (x$name %in% c("script", "style") || identical(x$attribs$translate, "no")) return(x)  # translate = "no": data, keep as stored
    x$children <- lapply(x$children, .tr_walk, d)
    for (a in intersect(names(x$attribs), c("title", "placeholder", "aria-label")))
      if (is.character(x$attribs[[a]])) x$attribs[[a]] <- .tr_walk(x$attribs[[a]], d)
    return(x)
  }
  if (is.list(x) && !is.data.frame(x)) {
    at <- attributes(x)
    x <- lapply(x, .tr_walk, d)
    attributes(x) <- at
  }
  x
}

# Shiny's own functions, wrapped so their text follows the session's language.
renderUI <- function(expr, env = parent.frame(), quoted = FALSE, outputArgs = list()) {
  force(env)
  if (!quoted) expr <- substitute(expr)
  shiny::renderUI(tr_tags(eval(expr, env)), outputArgs = outputArgs)
}
showNotification <- function(ui, ...) shiny::showNotification(if (is.character(ui)) tr(ui) else tr_tags(ui), ...)
showModal <- function(ui, ...) shiny::showModal(tr_tags(ui), ...)

# Dates: "08 Oct 2026" in English, "08/10/2026" in Vietnamese, "08 oct. 2026" in French, "2026年10月08日" in Chinese.
MONTHS_FR <- c("janv.", "févr.", "mars", "avr.", "mai", "juin", "juil.", "août", "sept.", "oct.", "nov.", "déc.")
MONTHS_EN <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
local_date <- function(x, time = NULL) {
  lang <- current_lang()
  day <- format(x, "%d"); m <- as.integer(format(x, "%m")); y <- format(x, "%Y")
  out <- switch(lang, vi = paste0(day, "/", format(x, "%m"), "/", y), fr = paste(day, MONTHS_FR[m], y), zh = paste0(y, "年", format(x, "%m"), "月", day, "日"), paste(day, MONTHS_EN[m], y))
  if (is.null(time)) out else paste(out, time)
}
