# Business rules: performance maths, holdings, composition, notifications and every action
# users can take. Actions call fail() with a message the UI shows as an error.

ROLES <- c(superadmin = "Super Administrator", fund_manager = "Fund Manager", fund_trader = "Fund Trader",
           investor = "Investor", introducer = "Introducer")
STAFF <- c("superadmin", "fund_manager", "fund_trader")
ACCESS_GROUPS <- c("GLOBAL", "VIETNAM")

fail <- function(msg) stop(structure(class = c("fms_error", "error", "condition"), list(message = tr(msg), call = NULL)))

add_days <- function(d, n) format(as.Date(d) + n)
add_months <- function(d, n) {
  d <- as.Date(d)
  y <- as.integer(format(d, "%Y")); m <- as.integer(format(d, "%m")) + n; day <- as.integer(format(d, "%d"))
  y <- y + (m - 1) %/% 12; m <- (m - 1) %% 12 + 1
  last <- as.integer(format(as.Date(sprintf("%04d-%02d-01", y + (m == 12), m %% 12 + 1)) - 1, "%d"))
  sprintf("%04d-%02d-%02d", y, m, pmin(day, last))
}
title_case <- function(s) paste(vapply(strsplit(s, "\\s+")[[1]], function(w) paste0(substr(w, 1, 1), tolower(substring(w, 2))), ""), collapse = " ")
valid_email <- function(e) grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", e)
nz <- function(x, alt = NA) if (is.null(x) || length(x) == 0 || (length(x) == 1 && (is.na(x) || identical(x, "")))) alt else x

audit <- function(user_id, action, detail) {
  run("INSERT INTO fms_audit_log (user_id, action, detail, created_at) VALUES (?, ?, ?, ?)", user_id, action, detail, now_iso())
}

user_by_id <- function(id) q1("SELECT * FROM fms_users WHERE user_id = ?", id)
# A user can hold several roles; u$role is the one in use, and the account menu switches between them.
roles_of <- function(user) { r <- strsplit(nz(user$roles, nz(user$role, "")), ",")[[1]]; intersect(names(ROLES), c(r, user$role)) }
has_role <- function(user, role) role %in% roles_of(user)
# SQL condition: the user holds this role (whether or not it is the one in use).
ROLE_SQL <- "(',' || COALESCE(roles, role) || ',') LIKE '%%,%s,%%'"
role_sql <- function(role, alias = "") sprintf(gsub("roles|role", paste0(alias, "\\0"), ROLE_SQL), role)
switch_role <- function(user, role) {
  if (!has_role(user, role)) fail("You do not have this role.")
  run("UPDATE fms_users SET role = ? WHERE user_id = ?", role, user$user_id)
  audit(user$user_id, "user.role", sprintf("Switched to %s", tolower(ROLES[[role]])))
}
access_of <- function(user) strsplit(nz(user$fund_access, ""), ",")[[1]]
can_see <- function(user, country) user$role == "superadmin" | country %in% access_of(user)

latest_date <- function() q1("SELECT MAX(price_date) AS d FROM fms_funds_prices")$d
fx_latest <- function() {
  r <- function(pair) q1("SELECT rate FROM fms_fx_rates WHERE pair = ? ORDER BY price_date DESC LIMIT 1", pair)$rate
  list(USDVND = r("USDVND"), EURUSD = r("EURUSD"))
}
to_usd <- function(amount, ccy, fx) ifelse(ccy == "USD", amount, ifelse(ccy == "EUR", amount * fx$EURUSD, amount / fx$USDVND))

# ---------- performance maths
# Fund prices never change inside the app, so series and metrics are cached per process.
.cache <- new.env()
cached <- function(key, expr) { if (is.null(.cache[[key]])) .cache[[key]] <- force(expr); .cache[[key]] }
fund_series <- function(fund_id) {
  key <- paste0("s", fund_id)
  if (is.null(.cache[[key]])) {
    s <- q("SELECT price_date AS d, nav_per_unit AS v, total_nav AS aum FROM fms_funds_prices WHERE fund_id = ? ORDER BY price_date", fund_id)
    s$date <- as.Date(s$d)
    .cache[[key]] <- s
  }
  .cache[[key]]
}
value_at <- function(s, date) {
  i <- findInterval(as.numeric(as.Date(date)), as.numeric(s$date))
  if (i < 1) NA_real_ else s$v[i]
}
metrics <- function(s) {
  n <- nrow(s); last <- s$v[n]; last_d <- s$d[n]
  ret <- function(from) { p <- value_at(s, from); if (is.na(p)) NA_real_ else last / p - 1 }
  ann <- function(months) { r <- ret(add_months(last_d, -months)); if (is.na(r)) NA_real_ else (1 + r)^(12 / months) - 1 }
  year <- as.integer(substr(last_d, 1, 4))
  year_ago <- s[s$d > add_months(last_d, -12), ]
  rets <- diff(log(year_ago$v))
  vol <- sd(rets) * sqrt(252)
  mdd <- min(s$v / cummax(s$v) - 1)
  years <- as.numeric(s$date[n] - s$date[1]) / 365.25
  m12 <- ret(add_months(last_d, -12))
  list(nav = last, nav_date = last_d, aum = s$aum[n], d1 = if (n > 1) last / s$v[n - 1] - 1 else NA,
       m1 = ret(add_months(last_d, -1)), m3 = ret(add_months(last_d, -3)), m6 = ret(add_months(last_d, -6)),
       ytd = ret(sprintf("%d-12-31", year - 1)), m12 = m12, m24a = ann(24), m36a = ann(36),
       since_start = last / s$v[1] - 1, since_start_ann = if (years > 1) (last / s$v[1])^(1 / years) - 1 else NA,
       history_start = s$d[1], vol_1y = vol, max_drawdown = mdd, sharpe_1y = if (is.na(m12)) NA else (m12 - 0.04) / vol)
}
fund_metrics <- function(fund_id) {
  key <- paste0("m", fund_id)
  if (is.null(.cache[[key]])) .cache[[key]] <- metrics(fund_series(fund_id))
  .cache[[key]]
}

# The fund list and positions are not edited in the app, so the full table is built once; each user gets their filtered view.
fund_rows <- function(user = NULL) {
  all <- cached("fund_rows", {
    f <- q("SELECT f.*, (SELECT COUNT(*) FROM fms_fund_positions p WHERE p.fund_id = f.fund_id AND p.instrument_type NOT IN ('cash','tbill')) AS components
            FROM fms_fund_list f ORDER BY f.country, f.fund_id")
    m <- do.call(rbind, lapply(f$fund_id, function(id) as.data.frame(fund_metrics(id))))
    out <- cbind(f, m)
    out$aum_usd <- to_usd(out$aum, out$currency, fx_latest())
    out
  })
  if (is.null(user)) all else all[can_see(user, all$country), ]
}

fund_by_code <- function(code, user = NULL) {
  f <- q1("SELECT * FROM fms_fund_list WHERE fund_code = ?", code)
  if (is.null(f)) fail(sprintf(tr("No fund with code %s."), code))
  if (!is.null(user) && !can_see(user, f$country))
    fail(sprintf(tr("Your account does not have access to %s funds."), if (f$country == "VIETNAM") "Vietnam" else "Global"))
  f
}

benchmark_series <- function(code, from) {
  b <- q("SELECT price_date AS d, close_value AS v FROM fms_benchmark WHERE benchmark_code = ? AND price_date >= ? ORDER BY price_date", code, from)
  b$date <- as.Date(b$d); b$aum <- rep(0, nrow(b))
  b
}

# Everything a fund can be compared with: the benchmark indices, then market indices, commodities and crypto.
benchmark_choices <- function() {
  b <- q("SELECT DISTINCT benchmark_code, benchmark_name FROM fms_benchmark ORDER BY benchmark_name")
  m <- q("SELECT symbol, name, asset_class FROM fms_market_list WHERE asset_class IN ('Indices', 'Commodities', 'Crypto') ORDER BY asset_class DESC, name")
  m <- m[!m$symbol %in% b$benchmark_code, ]
  list(`Benchmark indices` = setNames(b$benchmark_code, b$benchmark_name),
       Markets = setNames(m$symbol, paste0(m$name, " (", m$symbol, ")")))
}
# Default comparison: the VN-Index for Vietnam funds, the S&P 500 for global funds.
default_comparison <- function(f) if (identical(toupper(f$country), "VIETNAM")) "VNINDEX" else "SPX"
comparison_name <- function(code) {
  n <- q1("SELECT benchmark_name AS n FROM fms_benchmark WHERE benchmark_code = ? LIMIT 1", code)
  if (is.null(n)) n <- q1("SELECT name AS n FROM fms_market_list WHERE symbol = ?", code)
  if (is.null(n)) code else n$n
}
# Series for any comparison code: a benchmark index if there is one, otherwise a market's closing levels.
comparison_series <- function(code, from) {
  b <- benchmark_series(code, from)
  if (nrow(b)) return(b)
  m <- q("SELECT price_date AS d, close AS v FROM fms_market_prices WHERE symbol = ? AND price_date >= ? ORDER BY price_date", code, from)
  m$date <- as.Date(m$d); m$aum <- rep(0, nrow(m))
  m
}

calendar_returns <- function(s, b) {
  years <- unique(substr(s$d, 1, 4))
  yr <- function(arr, y) {
    end <- value_at(arr, sprintf("%s-12-31", y)); start <- value_at(arr, sprintf("%d-12-31", as.integer(y) - 1))
    if (is.na(start)) start <- arr$v[1]
    if (is.na(end)) NA else end / start - 1
  }
  data.frame(year = years, fund = vapply(years, function(y) yr(s, y), 0), benchmark = vapply(years, function(y) yr(b, y), 0),
             partial = (years == substr(s$d[1], 1, 4) & s$d[1] > paste0(years, "-01-06")) | years == substr(s$d[nrow(s)], 1, 4))
}

# ---------- composition
composition <- function(f) {
  nav <- q1("SELECT total_nav FROM fms_funds_prices WHERE fund_id = ? ORDER BY price_date DESC LIMIT 1", f$fund_id)$total_nav
  p <- q("SELECT * FROM fms_fund_positions WHERE fund_id = ? ORDER BY instrument_type, sector, contract", f$fund_id)
  p$exposure <- p$contracts * p$price * p$multiplier * p$fx_rate
  p$weight <- p$exposure / nav
  p$margin <- abs(p$contracts) * p$margin_per_contract
  group <- function(key) {
    g <- do.call(rbind, lapply(split(p, p[[key]]), function(x) data.frame(
      name = x[[key]][1], long = sum(x$weight[x$exposure >= 0]), short = sum(x$weight[x$exposure < 0]),
      net = sum(x$weight), gross = sum(abs(x$weight)))))
    g[order(-g$gross), ]
  }
  fut <- p[p$instrument_type == "future", ]
  list(nav = nav, currency = f$currency, positions = p, by_sleeve = group("sleeve"), by_sector = group("sector"),
       gross_exposure = if (nrow(fut)) sum(abs(fut$weight)) else sum(p$weight[p$instrument_type != "cash"]),
       net_exposure = if (nrow(fut)) sum(fut$weight) else NA,
       liquidity = sum(p$weight[p$instrument_type %in% c("cash", "tbill", "deposit")]),
       margin_used = sum(fut$margin))
}

# ---------- investor holdings (average-cost method)
holdings <- function(investor_id) {
  t <- q("SELECT t.*, f.fund_code, f.fund_name, f.currency FROM fms_transactions t JOIN fms_fund_list f USING (fund_id)
          WHERE t.investor_id = ? AND t.status = 'settled' ORDER BY t.trade_date, t.transaction_id", investor_id)
  empty <- data.frame(fund_id = integer(), fund_code = character(), fund_name = character(), currency = character(), units = numeric(),
                      cost = numeric(), first_date = character(), nav = numeric(), nav_date = character(), value = numeric(),
                      pnl = numeric(), pnl_pct = numeric(), value_usd = numeric(), pnl_usd = numeric())
  if (!nrow(t)) return(empty)
  fx <- fx_latest()
  rows <- lapply(split(t, t$fund_id), function(x) {
    units <- 0; cost <- 0
    for (i in seq_len(nrow(x))) {
      if (x$tx_type[i] == "redemption") { avg <- if (units) cost / units else 0; cost <- cost - avg * x$units[i]; units <- units - x$units[i] }
      else { units <- units + x$units[i]; cost <- cost + x$amount[i] }
    }
    if (units <= 1e-6) return(NULL)
    last <- q1("SELECT nav_per_unit, price_date FROM fms_funds_prices WHERE fund_id = ? ORDER BY price_date DESC LIMIT 1", x$fund_id[1])
    value <- units * last$nav_per_unit
    data.frame(fund_id = x$fund_id[1], fund_code = x$fund_code[1], fund_name = x$fund_name[1], currency = x$currency[1],
               units = round(units, 4), cost = round(cost, 2), first_date = x$trade_date[1], nav = last$nav_per_unit,
               nav_date = last$price_date, value = round(value, 2), pnl = round(value - cost, 2),
               pnl_pct = if (cost) value / cost - 1 else NA, value_usd = to_usd(value, x$currency[1], fx),
               pnl_usd = to_usd(value - cost, x$currency[1], fx))
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) empty else out
}
units_held <- function(investor_id, fund_id) { h <- holdings(investor_id); u <- h$units[h$fund_id == fund_id]; if (length(u)) u else 0 }

investor_transactions <- function(where, ...) q(paste0(
  "SELECT t.*, f.fund_code, f.fund_name, f.currency, u.display_name AS investor_name, u.email AS investor_email
   FROM fms_transactions t JOIN fms_fund_list f USING (fund_id) JOIN fms_users u ON u.user_id = t.investor_id
   WHERE ", where, " ORDER BY COALESCE(t.trade_date, t.order_date) DESC, t.transaction_id DESC"), ...)

# ---------- notifications and badge counts
notifications <- function(user) {
  items <- list()
  add <- function(kind, text, at, link) items[[length(items) + 1]] <<- list(kind = kind, text = text, at = at, link = link)
  if (user$role == "investor") {
    p <- investor_transactions("t.investor_id = ? AND t.status = 'pending'", user$user_id)
    for (i in seq_len(nrow(p))) add("order", sprintf(tr("Your %s order for %s is waiting for processing."), p$tx_type[i], p$fund_code[i]), p$order_date[i], "room")
    d <- q("SELECT d.*, f.fund_code, f.currency FROM fms_dividends d JOIN fms_fund_list f USING (fund_id) WHERE investor_id = ? ORDER BY payment_date DESC LIMIT 2", user$user_id)
    for (i in seq_len(nrow(d))) add("dividend", sprintf(tr("Dividend of %s %s from %s %s."), fmt_num(d$net_amount[i], if (d$currency[i] == "VND") 0 else 2),
                                                        d$currency[i], d$fund_code[i], if (d$status[i] == "paid") "paid" else "declared"), d$payment_date[i], "room")
  }
  if (user$role == "superadmin") {
    u <- q("SELECT display_name, role, created_at FROM fms_users WHERE status = 'pending' ORDER BY created_at DESC")
    for (i in seq_len(nrow(u))) add("signup", sprintf(tr("%s signed up as %s and needs approval."), u$display_name[i], tolower(ROLES[[u$role[i]]])), substr(u$created_at[i], 1, 10), "users")
    p <- investor_transactions("t.status = 'pending'")
    for (i in seq_len(nrow(p))) add("order", sprintf(tr("%s: %s in %s needs approval."), p$investor_name[i], p$tx_type[i], p$fund_code[i]), p$order_date[i], "backoffice")
    r <- q("SELECT r.subject, r.created_at, u.display_name FROM fms_requests r JOIN fms_users u USING (user_id) WHERE r.status = 'open' ORDER BY r.created_at DESC")
    for (i in seq_len(nrow(r))) add("request", sprintf(tr("%s sent a request: %s"), r$display_name[i], r$subject[i]), substr(r$created_at[i], 1, 10), "contact")
  }
  if (user$role %in% STAFF) {
    o <- q("SELECT o.*, f.fund_code FROM fms_trade_orders o JOIN fms_fund_list f USING (fund_id) WHERE o.status = 'working'")
    for (i in seq_len(nrow(o))) add("trade", sprintf(tr("Working %s order: %s × %s for %s."), o$side[i], o$contracts[i], o$contract[i], o$fund_code[i]), substr(o$created_at[i], 1, 10), "trading")
  }
  if (user$role == "introducer") {
    c <- q("SELECT period, COUNT(*) AS n FROM fms_commissions WHERE introducer_id = ? AND status = 'accrued' GROUP BY period", user$user_id)
    for (i in seq_len(nrow(c))) add("commission", sprintf(tr("Trail commission for %s accrued on %d holdings."), c$period[i], c$n[i]), paste0(c$period[i], "-30"), "room")
  }
  if (length(items)) items[order(vapply(items, function(x) as.character(x$at), ""), decreasing = TRUE)] else items
}

badge_counts <- function(user) {
  n <- function(sql) q1(sql)$n
  list(pending_orders = if (user$role == "superadmin") n("SELECT COUNT(*) AS n FROM fms_transactions WHERE status = 'pending'") else 0,
       pending_signups = if (user$role == "superadmin") n("SELECT COUNT(*) AS n FROM fms_users WHERE status = 'pending'") else 0,
       open_requests = if (user$role == "superadmin") n("SELECT COUNT(*) AS n FROM fms_requests WHERE status = 'open'") else 0,
       working_trades = if (user$role %in% STAFF) n("SELECT COUNT(*) AS n FROM fms_trade_orders WHERE status = 'working'") else 0)
}

# ---------- markets monitor (independent of the funds)
# Market prices are static too, so the table and each series are computed once per process.
market_series <- function(symbol) cached(paste0("mk_", symbol), {
  s <- q("SELECT price_date AS d, close AS v FROM fms_market_prices WHERE symbol = ? ORDER BY price_date", symbol)
  s$date <- as.Date(s$d)
  s
})
# When the market prices were last updated: the time data/rds/fms_market_prices.rds was written, in Vietnam time.
market_updated <- function() {
  f <- file.path("data", "rds", "fms_market_prices.rds")
  t <- if (file.exists(f)) file.mtime(f) else file.mtime(file.path("data", "fms-seed.db"))  # data/rds is not published
  format(t, "%Y-%m-%d %H:%M", tz = "Asia/Ho_Chi_Minh")
}
market_table <- function() cached("market_table", {
  list <- q("SELECT * FROM fms_market_list")
  order <- c("Indices", "Vietnam Stocks", "International Stocks", "Commodities", "Forex", "Rates", "Crypto")
  rows <- lapply(seq_len(nrow(list)), function(i) {
    mk <- list[i, ]
    s <- market_series(mk$symbol)
    n <- nrow(s); last <- s$v[n]
    chg <- function(from) { p <- value_at(s, from); if (is.na(p)) NA else last / p - 1 }
    cbind(mk, data.frame(last = last, date = s$d[n], d1 = last / s$v[n - 1] - 1, d1_abs = last - s$v[n - 1],
                         w1 = chg(add_days(s$d[n], -7)), m1 = chg(add_months(s$d[n], -1)),
                         ytd = chg(sprintf("%d-12-31", as.integer(substr(s$d[n], 1, 4)) - 1)), m12 = chg(add_months(s$d[n], -12))))
  })
  out <- do.call(rbind, rows)
  out[order(match(out$asset_class, order), seq_len(nrow(out))), ]
})

# ---------- supervision
compliance_checks <- function(funds) {
  do.call(rbind, lapply(seq_len(nrow(funds)), function(i) {
    f <- funds[i, ]
    spec <- q1("SELECT * FROM fms_funds_specs WHERE fund_id = ?", f$fund_id)
    c <- composition(f)
    pos <- c$positions[c$positions$instrument_type %in% c("future", "equity"), ]
    largest <- if (nrow(pos)) max(abs(pos$weight)) else 0
    rule <- function(name, value, limit, kind) {
      breach <- if (kind == "max") value > limit else value < limit
      near <- if (kind == "max") value > limit * 0.9 else value < limit * 1.1
      data.frame(fund_code = f$fund_code, fund_name = f$fund_name, rule = name, value = value, limit = limit, kind = kind,
                 state = if (breach) "breach" else if (near) "watch" else "ok")
    }
    rbind(rule("Gross exposure", c$gross_exposure, spec$max_gross_exposure_pct / 100, "max"),
          rule("Largest single position", largest, spec$max_single_market_pct / 100, "max"),
          rule("Liquidity", c$liquidity, spec$min_liquidity_pct / 100, "min"),
          rule("Volatility, 1 year", f$vol_1y, spec$target_volatility_pct / 100 * 1.5, "max"))
  }))
}

# ---------- actions
login <- function(email, password) {
  u <- q1("SELECT * FROM fms_users WHERE lower(email) = ?", tolower(trimws(email)))
  if (is.null(u) || !verify_password(password, u$password_hash)) fail("Email or password is incorrect.")
  if (u$status == "pending") fail("Your sign-up is waiting for approval by the administrator. You can sign in once it is approved.")
  if (u$status == "rejected") fail("Your sign-up was not approved. Contact the administrator for details.")
  if (u$status != "active") fail("This account is suspended. Contact the administrator.")
  if (!startsWith(u$password_hash, "$7$")) run("UPDATE fms_users SET password_hash = ? WHERE user_id = ?", hash_password(password), u$user_id)
  run("UPDATE fms_users SET last_login_at = ? WHERE user_id = ?", now_iso(), u$user_id)
  audit(u$user_id, "user.login", "Signed in")
  user_by_id(u$user_id)
}

# ---------- staying signed in
# Each sign-in gets a random token kept by the browser, so a reload or a reconnection signs the user back in.
# With "Remember me" the token lasts 30 days on this browser, otherwise until the browser is closed (1 day at most).
# fms_sessions keeps only the SHA-256 of the token.
token_hash <- function(token) paste(openssl::sha256(enc2utf8(token)))
session_start <- function(user_id, days = 30) {
  run("DELETE FROM fms_sessions WHERE expires_at < ?", now_iso())
  token <- paste(openssl::rand_bytes(32), collapse = "")
  run("INSERT INTO fms_sessions (token, user_id, created_at, expires_at) VALUES (?, ?, ?, ?)", token_hash(token), user_id, now_iso(),
      format(Sys.time() + days * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  token
}
session_resume <- function(token) {
  if (!is.character(token) || length(token) != 1 || nchar(token) != 64) return(NULL)
  s <- q1("SELECT * FROM fms_sessions WHERE token = ? AND expires_at > ?", token_hash(token), now_iso())
  if (is.null(s)) return(NULL)
  u <- user_by_id(s$user_id)
  if (is.null(u) || u$status != "active") return(NULL)
  u
}
session_end <- function(token) if (is.character(token) && length(token) == 1) run("DELETE FROM fms_sessions WHERE token = ?", token_hash(token))

signup <- function(b) {
  email <- tolower(trimws(nz(b$email, "")))
  if (!valid_email(email)) fail("Enter a valid email address.")
  first <- toupper(trimws(nz(b$first_name, ""))); last <- toupper(trimws(nz(b$last_name, "")))
  if (first == "" || last == "") fail("First name and last name are required.")
  role <- if (identical(b$role, "introducer")) "introducer" else "investor"
  if (nchar(nz(b$password, "")) < 6) fail("Password must be at least 6 characters.")
  if (!identical(b$password, b$confirm_password)) fail("Password and confirmation do not match.")
  if (!is.null(q1("SELECT 1 AS x FROM fms_users WHERE lower(email) = ?", email))) fail("An account with this email already exists.")
  intro_id <- NA
  if (role == "investor" && nz(b$introducer_email, "") != "") {
    i <- q1(paste("SELECT user_id FROM fms_users WHERE lower(email) = ? AND status = 'active' AND", role_sql("introducer")), tolower(trimws(b$introducer_email)))
    if (is.null(i)) fail("No introducer has that email. Check it or leave the field empty.")
    intro_id <- i$user_id
  }
  run("INSERT INTO fms_users (email, password_hash, title, first_name, last_name, display_name, role, roles, phone, country, status, created_at, introducer_id)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'pending', ?, ?)",
      email, hash_password(b$password), nz(b$title), first, last, paste(last, title_case(first)), role, role, nz(b$phone), nz(b$country), now_iso(), intro_id)
  audit(last_id(), "user.signup", sprintf("Signed up as %s (waiting for approval)", tolower(ROLES[[role]])))
}

update_profile <- function(user, b) {
  email <- trimws(nz(b$email, ""))
  if (!valid_email(email)) fail("Enter a valid email address.")
  if (!is.null(q1("SELECT 1 AS x FROM fms_users WHERE lower(email) = ? AND user_id <> ?", tolower(email), user$user_id))) fail("Another account already uses this email.")
  first <- trimws(nz(b$first_name, "")); last <- trimws(nz(b$last_name, ""))
  if (first == "" || last == "") fail("First name and last name are required.")
  dob <- nz(b$date_of_birth)
  if (nz(b$new_password, "") != "") {
    if (!verify_password(nz(b$current_password, ""), user$password_hash)) fail("Current password is incorrect.")
    if (nchar(b$new_password) < 6) fail("New password must be at least 6 characters.")
    if (!identical(b$new_password, b$confirm_password)) fail("New password and confirmation do not match.")
    run("UPDATE fms_users SET password_hash = ? WHERE user_id = ?", hash_password(b$new_password), user$user_id)
    audit(user$user_id, "user.password", "Changed password")
  }
  avatar <- if (isTRUE(b$remove_avatar)) NA else nz(b$avatar, nz(user$avatar))
  run("UPDATE fms_users SET email = ?, title = ?, first_name = ?, last_name = ?, display_name = ?, avatar = ?, phone = ?, country = ?,
       date_of_birth = ?, nationality = ?, academic = ? WHERE user_id = ?",
      email, nz(b$title), first, last, nz(b$display_name, paste(last, first)), avatar, nz(b$phone), nz(b$country), dob,
      nz(b$nationality), nz(b$academic), user$user_id)
  audit(user$user_id, "user.profile", "Updated profile")
}

place_order <- function(user, fund_code, tx_type, amount = NA, units = NA) {
  f <- fund_by_code(fund_code, user)
  spec <- q1("SELECT * FROM fms_funds_specs WHERE fund_id = ?", f$fund_id)
  if (f$status != "open") fail(sprintf(tr("%s is not open for dealing."), f$fund_code))
  if (tx_type == "subscription") {
    held <- units_held(user$user_id, f$fund_id) > 0
    min <- if (held) spec$min_subsequent else spec$min_initial_investment
    if (is.na(amount) || amount < min) fail(sprintf(tr("Minimum %s subscription is %s %s."), if (held) "additional" else "initial", fmt_num(min, 0), f$currency))
    fee <- round(amount * spec$subscription_fee_pct / 100, if (f$currency == "VND") 0 else 2)
    run("INSERT INTO fms_transactions (investor_id, fund_id, tx_type, order_date, amount, fee, status, note) VALUES (?, ?, 'subscription', ?, ?, ?, 'pending', 'Online order')",
        user$user_id, f$fund_id, today_iso(), amount, fee)
    audit(user$user_id, "order.create", sprintf("Subscription of %s %s into %s", fmt_num(amount, 0), f$currency, f$fund_code))
  } else {
    pending <- q1("SELECT COALESCE(SUM(units), 0) AS u FROM fms_transactions WHERE investor_id = ? AND fund_id = ? AND tx_type = 'redemption' AND status = 'pending'", user$user_id, f$fund_id)$u
    available <- units_held(user$user_id, f$fund_id) - pending
    if (is.na(units) || units <= 0) fail("Enter the number of units to redeem.")
    if (units > available + 1e-6) fail(sprintf(tr("You can redeem up to %s units of %s."), fmt_num(max(available, 0), 4), f$fund_code))
    run("INSERT INTO fms_transactions (investor_id, fund_id, tx_type, order_date, units, fee, status, note) VALUES (?, ?, 'redemption', ?, ?, 0, 'pending', 'Online order')",
        user$user_id, f$fund_id, today_iso(), units)
    audit(user$user_id, "order.create", sprintf("Redemption of %s units of %s", units, f$fund_code))
  }
}

cancel_order <- function(user, id) {
  t <- q1("SELECT * FROM fms_transactions WHERE transaction_id = ? AND investor_id = ?", id, user$user_id)
  if (is.null(t)) fail("Order not found.")
  if (t$status != "pending") fail("Only pending orders can be cancelled.")
  run("UPDATE fms_transactions SET status = 'cancelled', note = 'Cancelled by investor' WHERE transaction_id = ?", id)
  audit(user$user_id, "order.cancel", sprintf("Cancelled order #%s", id))
}

decide_signup <- function(admin, id, decision) {
  t <- user_by_id(id)
  if (is.null(t)) fail("User not found.")
  if (t$status != "pending") fail("This sign-up has already been decided.")
  status <- if (decision == "approve") "active" else "rejected"
  run("UPDATE fms_users SET status = ?, approved_by = ?, approved_at = ? WHERE user_id = ?", status, admin$user_id, now_iso(), id)
  audit(admin$user_id, paste0("user.", decision), sprintf("%s sign-up of %s", if (status == "active") "Approved" else "Rejected", t$email))
}

save_user <- function(admin, b, id = NULL) {
  email <- tolower(trimws(nz(b$email, "")))
  if (!valid_email(email)) fail("Enter a valid email address.")
  first <- toupper(trimws(nz(b$first_name, ""))); last <- toupper(trimws(nz(b$last_name, "")))
  if (first == "" || last == "") fail("First name and last name are required.")
  roles <- intersect(names(ROLES), b$roles)
  if (!length(roles)) fail("Choose at least one role.")
  access <- paste(intersect(ACCESS_GROUPS, b$fund_access), collapse = ",")
  pw <- nz(b$password, "")
  if ((is.null(id) || pw != "") && nchar(pw) < 6) fail("Password must be at least 6 characters.")
  intro <- if ("investor" %in% roles && nz(b$introducer_id, "") != "") as.integer(b$introducer_id) else NA
  display <- nz(b$display_name, paste(last, title_case(first)))
  if (is.null(id)) {
    if (!is.null(q1("SELECT 1 AS x FROM fms_users WHERE lower(email) = ?", email))) fail("An account with this email already exists.")
    now <- now_iso()
    run("INSERT INTO fms_users (email, password_hash, title, first_name, last_name, display_name, role, roles, phone, country, introducer_id, fund_access, status, approved_by, approved_at, created_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'active', ?, ?, ?)",
        email, hash_password(pw), nz(b$title), first, last, display, roles[1], paste(roles, collapse = ","), nz(b$phone), nz(b$country), intro, access, admin$user_id, now, now)
    audit(admin$user_id, "user.create", sprintf("Created %s account %s", tolower(paste(unlist(ROLES[roles]), collapse = " + ")), email))
  } else {
    t <- user_by_id(id)
    if (!is.null(q1("SELECT 1 AS x FROM fms_users WHERE lower(email) = ? AND user_id <> ?", email, id))) fail("Another account already uses this email.")
    status <- if (nz(b$status, "") %in% c("pending", "active", "suspended", "rejected")) b$status else t$status
    if (id == admin$user_id && (status != "active" || !"superadmin" %in% roles)) fail("You cannot change your own role or status.")
    role <- if (t$role %in% roles) t$role else roles[1]  # the role in use stays, unless it was taken away
    approved <- status == "active" && t$status == "pending"
    run("UPDATE fms_users SET email = ?, title = ?, first_name = ?, last_name = ?, display_name = ?, role = ?, roles = ?, phone = ?, country = ?, introducer_id = ?,
         fund_access = ?, status = ? WHERE user_id = ?", email, nz(b$title), first, last, display, role, paste(roles, collapse = ","), nz(b$phone), nz(b$country), intro, access, status, id)
    if (approved) run("UPDATE fms_users SET approved_by = ?, approved_at = ? WHERE user_id = ?", admin$user_id, now_iso(), id)
    if (pw != "") run("UPDATE fms_users SET password_hash = ? WHERE user_id = ?", hash_password(pw), id)
    audit(admin$user_id, "user.update", sprintf("Updated account %s%s", email, if (pw != "") " (password reset)" else ""))
  }
}

trade_order <- function(user, b) {
  f <- fund_by_code(nz(b$fund_code, ""))
  pos <- q1("SELECT * FROM fms_fund_positions WHERE fund_id = ? AND contract = ? AND instrument_type = 'future'", f$fund_id, nz(b$contract, ""))
  if (is.null(pos)) fail(sprintf(tr("%s is not in the %s universe."), nz(b$contract, "That contract"), f$fund_code))
  qty <- floor(as.numeric(nz(b$contracts, 0)))
  if (is.na(qty) || qty < 1 || qty > 500) fail("Contracts must be a whole number between 1 and 500.")
  side <- if (identical(b$side, "sell")) "sell" else "buy"
  type <- if (identical(b$order_type, "market")) "market" else "limit"
  limit <- if (type == "limit") as.numeric(nz(b$limit_price, NA)) else NA
  if (type == "limit" && (is.na(limit) || limit <= 0)) fail("Enter a limit price.")
  now <- now_iso()
  run("INSERT INTO fms_trade_orders (fund_id, contract, side, contracts, order_type, limit_price, filled_contracts, avg_fill_price, status, trader_id, created_at, updated_at, note)
       VALUES (?, ?, ?, ?, ?, ?, 0, NULL, 'working', ?, ?, ?, ?)", f$fund_id, pos$contract, side, qty, type, limit, user$user_id, now, now, nz(b$note))
  audit(user$user_id, "order.create", sprintf("%s %s × %s for %s", side, qty, pos$contract, f$fund_code))
}

trade_action <- function(user, id, action) {
  o <- q1("SELECT * FROM fms_trade_orders WHERE order_id = ?", id)
  if (is.null(o)) fail("Order not found.")
  if (o$status != "working") fail("Only working orders can be changed.")
  now <- now_iso()
  if (action == "cancel") {
    run("UPDATE fms_trade_orders SET status = 'cancelled', updated_at = ? WHERE order_id = ?", now, id)
    audit(user$user_id, "order.cancel", sprintf("Cancelled %s order #%s", o$contract, id))
    return(invisible())
  }
  pos <- q1("SELECT * FROM fms_fund_positions WHERE fund_id = ? AND contract = ?", o$fund_id, o$contract)
  price <- if (!is.na(o$limit_price)) o$limit_price else pos$price
  tx({
    run("UPDATE fms_trade_orders SET status = 'filled', filled_contracts = contracts, avg_fill_price = ?, updated_at = ? WHERE order_id = ?", price, now, id)
    run("UPDATE fms_fund_positions SET contracts = contracts + ?, price = ? WHERE position_id = ?", if (o$side == "buy") o$contracts else -o$contracts, price, pos$position_id)
  })
  audit(user$user_id, "order.fill", sprintf("Filled %s %s × %s at %s", o$side, o$contracts, o$contract, price))
}

process_transaction <- function(admin, id, action, reason = "Rejected by back office") {
  t <- q1("SELECT t.*, f.fund_code, f.currency FROM fms_transactions t JOIN fms_fund_list f USING (fund_id) WHERE transaction_id = ?", id)
  if (is.null(t)) fail("Transaction not found.")
  if (t$status != "pending") fail("This order has already been processed.")
  if (action == "reject") {
    run("UPDATE fms_transactions SET status = 'rejected', processed_by = ?, note = ? WHERE transaction_id = ?", admin$user_id, reason, id)
    audit(admin$user_id, "order.reject", sprintf("Rejected order #%s (%s)", id, t$fund_code))
    return(invisible())
  }
  p <- q1("SELECT nav_per_unit, price_date FROM fms_funds_prices WHERE fund_id = ? ORDER BY price_date DESC LIMIT 1", t$fund_id)
  if (t$tx_type == "subscription") {
    run("UPDATE fms_transactions SET status = 'settled', trade_date = ?, units = ?, nav_per_unit = ?, processed_by = ? WHERE transaction_id = ?",
        p$price_date, round((t$amount - t$fee) / p$nav_per_unit, 4), p$nav_per_unit, admin$user_id, id)
  } else {
    if (t$units > units_held(t$investor_id, t$fund_id) + 1e-6) fail("The investor no longer holds enough units for this redemption.")
    run("UPDATE fms_transactions SET status = 'settled', trade_date = ?, amount = ?, nav_per_unit = ?, processed_by = ? WHERE transaction_id = ?",
        p$price_date, round(t$units * p$nav_per_unit, if (t$currency == "VND") 0 else 2), p$nav_per_unit, admin$user_id, id)
  }
  audit(admin$user_id, "order.approve", sprintf("Settled order #%s (%s) at NAV %s of %s", id, t$fund_code, p$nav_per_unit, p$price_date))
}

declare_dividend <- function(admin, fund_code, per_unit, record, pay) {
  f <- fund_by_code(fund_code)
  if (is.na(per_unit) || per_unit <= 0) fail("Enter the dividend per unit.")
  if (!is.null(q1("SELECT 1 AS x FROM fms_dividends WHERE fund_id = ? AND record_date = ?", f$fund_id, record)))
    fail(sprintf(tr("A dividend with record date %s already exists for %s."), record, f$fund_code))
  inv <- q("SELECT DISTINCT investor_id FROM fms_transactions WHERE fund_id = ? AND status = 'settled'", f$fund_id)$investor_id
  dp <- if (f$currency == "VND") 0 else 2
  n <- 0
  tx(for (id in inv) {
    units <- units_held(id, f$fund_id)
    if (units <= 0) next
    gross <- round(units * per_unit, dp); tax <- round(gross * 0.05, dp)
    run("INSERT INTO fms_dividends (investor_id, fund_id, record_date, payment_date, per_unit, units, gross_amount, withholding_tax, net_amount, method, status)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 'cash', 'declared')", id, f$fund_id, record, pay, per_unit, units, gross, tax, round(gross - tax, dp))
    n <- n + 1
  })
  if (!n) fail(sprintf(tr("No investors hold %s, so there is nothing to declare."), f$fund_code))
  audit(admin$user_id, "dividend.declare", sprintf("Declared %s %s per unit on %s, record date %s", per_unit, f$currency, f$fund_code, record))
  n
}
