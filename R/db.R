# SQLite access. The working database is a copy of data/fms-seed.db, made on first start.
# Set FMS_DB to put it somewhere else; delete it to reset the demo.

db_path <- Sys.getenv("FMS_DB", file.path("data", "fms.db"))
dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
# Some hosts mount the app folder read-only; then work on a copy in the temp folder.
if (file.access(dirname(db_path), 2) != 0 || (file.exists(db_path) && file.access(db_path, 2) != 0)) db_path <- file.path(tempdir(), "fms.db")
if (!file.exists(db_path) && !file.copy(file.path("data", "fms-seed.db"), db_path)) stop("Could not create the database at ", db_path)
con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
# Each user's interface language and light/dark mode (added after the first release, so older databases get the columns here).
# Empty means "not chosen yet": the choice made on the sign-in screen is then kept.
for (col in c("lang TEXT", "theme TEXT"))
  if (!sub(" .*", "", col) %in% DBI::dbListFields(con, "fms_users")) DBI::dbExecute(con, paste("ALTER TABLE fms_users ADD COLUMN", col))
# Several roles per user (e.g. investor and introducer): "roles" lists them, "role" is the one in use now.
if (!"roles" %in% DBI::dbListFields(con, "fms_users")) {
  DBI::dbExecute(con, "ALTER TABLE fms_users ADD COLUMN roles TEXT")
  DBI::dbExecute(con, "UPDATE fms_users SET roles = role WHERE roles IS NULL OR roles = ''")
}
# Help > Contact: information requests sent by users (added after the first release, so older databases get the table here).
DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS fms_requests (
  request_id INTEGER PRIMARY KEY, user_id INTEGER NOT NULL REFERENCES fms_users(user_id), topic TEXT NOT NULL,
  fund_id INTEGER REFERENCES fms_fund_list(fund_id), subject TEXT NOT NULL, message TEXT NOT NULL,
  reply_by TEXT NOT NULL DEFAULT 'email' CHECK (reply_by IN ('email','phone')),
  status TEXT NOT NULL DEFAULT 'open' CHECK (status IN ('open','answered')),
  created_at TEXT NOT NULL, answered_by INTEGER REFERENCES fms_users(user_id), answered_at TEXT)")
message(sprintf("[FMS] database %s; R %s; shiny %s, bslib %s, RSQLite %s, sodium %s, plotly %s",
                normalizePath(db_path), getRversion(), packageVersion("shiny"), packageVersion("bslib"),
                packageVersion("RSQLite"), packageVersion("sodium"), packageVersion("plotly")))

# q() returns a data frame, q1() the first row as a list (or NULL), run() the rows changed.
# Fund names always show in capitals, everywhere in the app, so q() upper-cases them as they are read.
q <- function(sql, ...) {
  params <- list(...)
  d <- if (length(params)) DBI::dbGetQuery(con, sql, params = unname(params)) else DBI::dbGetQuery(con, sql)
  if ("fund_name" %in% names(d)) d$fund_name <- toupper(d$fund_name)
  d
}
q1 <- function(sql, ...) {
  d <- q(sql, ...)
  if (nrow(d)) as.list(d[1, , drop = FALSE]) else NULL
}
run <- function(sql, ...) {
  params <- lapply(list(...), function(p) if (is.null(p) || length(p) == 0) NA else p)
  if (length(params)) DBI::dbExecute(con, sql, params = unname(params)) else DBI::dbExecute(con, sql)
}
last_id <- function() DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
tx <- function(expr) DBI::dbWithTransaction(con, expr)

now_iso <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
today_iso <- function() format(Sys.Date())
