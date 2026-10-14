# Export every FMS table to an .rds file (demo data + structure), and import them back.
#   Rscript tools/export-rds.R                 -> data/rds/<table>.rds + data/rds/fms-structure.rds
#   Rscript tools/export-rds.R import [db]     -> rebuilds data/fms-seed.db (or [db]) from data/rds/*.rds
# In the .rds files dates are Date, timestamps are POSIXct (UTC) and fund_access is kept as text "GLOBAL,VIETNAM".
# Edit the data in R, saveRDS() it, run the import, and the website picks it up on next start.

suppressMessages({ library(DBI); library(RSQLite) })
args <- commandArgs(TRUE)
seed <- if (length(args) > 1) args[2] else "data/fms-seed.db"
dir <- "data/rds"

is_date <- function(n) grepl("(_date|^date_of_birth)$", n)
is_time <- function(n) grepl("(_at|^ts)$", n)

TABLES <- c(
  fms_users = "Accounts: superadmin, fund manager, trader, introducers and investors, with profile, role, status and fund access.",
  fms_sessions = "Sign-in tokens (stored as SHA-256) that keep a user signed in after a reload: 30 days with Remember me, otherwise until the browser closes.",
  fms_fund_list = "The funds: code, name, country (GLOBAL / VIETNAM), selection, strategy, currency, benchmark and manager.",
  fms_funds_specs = "Legal and dealing terms per fund: ISIN, structure, service providers, fees, cut-off and risk limits.",
  fms_funds_prices = "Daily NAV per unit, total NAV and units outstanding per fund.",
  fms_funds_intraday = "Today's indicative NAV (iNAV) every 5 minutes per fund.",
  fms_fund_positions = "Futures held by each fund: contract, sector, exchange, sleeve, contracts, price and margin.",
  fms_fund_documents = "Fund documents (factsheets, KID, prospectus, reports) with type, date, pages and language.",
  fms_benchmark = "Daily closing values of each fund's benchmark index.",
  fms_market_list = "Market instruments shown on the Markets page: symbol, name, asset class, region, unit.",
  fms_market_prices = "Daily closes for each market instrument.",
  fms_fx_rates = "Daily FX rates (pair such as EURUSD) used to convert AUM to USD.",
  fms_transactions = "Investor subscriptions and redemptions: dates, amount, units, NAV, fee and status.",
  fms_trade_orders = "Trader orders on futures: side, contracts, type, limit, fills, status and trader.",
  fms_dividends = "Dividends declared to investors: record and payment dates, per unit, gross, tax and net.",
  fms_commissions = "Introducer commissions per investor, fund and month: basis, rate, amount and status.",
  fms_audit_log = "Who did what and when (approvals, rebalances, fills, sign-ins).",
  fms_requests = "Information requests sent from Help > Contact: topic, fund, subject, message, how to reply and status.")

COLS <- c(
  topic = "Request topic, e.g. Fund information", subject = "Request subject", message = "Request text", reply_by = "How the user wants a reply: email or phone",
  answered_by = "user_id of the administrator who answered", 
  user_id = "User number (key)", email = "Sign-in e-mail (unique)", password_hash = "Hashed password (sodium); demo passwords are in tools/logins.json",
  role = "Role in use now: superadmin, fund_manager, fund_trader, introducer or investor", roles = "Every role the user holds, comma separated (e.g. investor,introducer)", status = "Record state (e.g. pending, active, suspended, rejected, settled, filled)",
  fund_access = "Countries the user may see, comma separated: GLOBAL, VIETNAM", introducer_id = "user_id of the introducer who brought the investor",
  fund_id = "Fund number (key, links to fms_fund_list)", fund_code = "Short fund code, e.g. GDC-ALL10", fund_name = "Full fund name",
  country = "Country / universe", currency = "Currency code (USD, EUR, VND)", price_date = "Price date", nav_per_unit = "NAV per unit in fund currency",
  total_nav = "Total net assets in fund currency", units_outstanding = "Units in issue", investor_id = "user_id of the investor",
  amount = "Amount in fund currency", units = "Number of units", fee = "Fee charged", created_at = "Created (UTC)", updated_at = "Last updated (UTC)",
  symbol = "Instrument symbol", close = "Closing price", pair = "Currency pair, e.g. EURUSD", rate = "Exchange rate", benchmark_code = "Benchmark index code",
  close_value = "Index close", contract = "Futures contract code", contracts = "Number of contracts", trader_id = "user_id of the trader",
  ts = "Timestamp (UTC)", inav = "Indicative NAV per unit", avatar = "Profile photo as data URL (PNG/JPEG, max 400 KB)",
  lang = "Preferred language: en, vi, fr, zh", theme = "Preferred mode: dark or light",
  tx_type = "subscription or redemption", order_date = "Date the order was placed", trade_date = "Date the order was dealt",
  side = "buy or sell", order_type = "market or limit", limit_price = "Limit price (limit orders)", period = "Month, YYYY-MM",
  rate_pct = "Rate in percent", basis_amount = "Amount the commission is calculated on", per_unit = "Dividend per unit",
  action = "Event code, e.g. fund.rebalance", detail = "What happened, in words", benchmark_name = "Benchmark index name",
  gross_amount = "Dividend before tax", withholding_tax = "Tax withheld", net_amount = "Dividend paid after tax", method = "cash or reinvest",
  doc_type = "Factsheet, KID, Prospectus, Annual report...", title = "Title (documents) or Mr/Ms (users)", pages = "Number of pages", language = "Document language",
  selection = "ACTIVE or COMBINED", ratio_label = "Allocation, e.g. 100% or 80%/20%", strategy_family = "Strategy family", asset_focus = "Asset focus",
  top_n = "Number of markets the model holds", risk_level = "Risk level 1 (low) to 7 (high)", share_class = "Accumulating or Distributing",
  objective = "Investment objective", strategy = "How the strategy works", instrument = "Exchange root symbol", description = "Instrument description",
  sector = "Sector", exchange = "Exchange", instrument_type = "future, cash...", sleeve = "Portfolio sleeve", price_currency = "Currency the price is quoted in",
  fx_rate = "FX rate to fund currency", price = "Last price", multiplier = "Contract multiplier", margin_per_contract = "Initial margin per contract",
  isin = "ISIN code", legal_structure = "Legal form", domicile = "Country of domicile", management_company = "Management company", custodian = "Custodian / depositary",
  clearing_broker = "Clearing broker", auditor = "Auditor", administrator = "Fund administrator", dealing_frequency = "How often units can be dealt",
  cut_off_time = "Order cut-off", settlement_cycle = "Settlement, e.g. T+2", min_initial_investment = "Minimum first investment", min_subsequent = "Minimum top-up",
  high_water_mark = "1 if the performance fee uses a high-water mark", distribution_policy = "Accumulating or Distributing", rebalancing = "Rebalancing schedule",
  name = "Instrument name", asset_class = "Asset class", region = "Region", quote_unit = "Quote unit, e.g. pts or USD/bbl", decimals = "Decimals to display",
  token = "Session token", filled_contracts = "Contracts filled so far", avg_fill_price = "Average fill price", note = "Free note",
  processed_by = "user_id who processed it", first_name = "First name", last_name = "Last name", display_name = "Name shown in the app", phone = "Phone",
  address = "Address", nationality = "Nationality", academic = "Professional / qualified investor (Yes/No)", id_document = "ID or passport number",
  bank_name = "Bank", bank_account = "Bank account", risk_profile = "Conservative, Balanced or Aggressive", approved_by = "user_id who approved the account")
# Columns without their own entry get a description from their name.
describe <- function(n) ifelse(!is.na(COLS[n]), COLS[n], ifelse(grepl("_id$", n), "Record number (links to its table)",
  ifelse(grepl("_pct$", n), "Percent", ifelse(is_date(n), "Date", ifelse(is_time(n), "Date and time (UTC)", "")))))

if (!identical(args[1], "import")) {
  con <- dbConnect(SQLite(), seed); on.exit(dbDisconnect(con))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  struct <- list()
  for (t in dbListTables(con)) {
    d <- dbReadTable(con, t)
    for (n in names(d)) {
      if (is_date(n)) d[[n]] <- as.Date(d[[n]])
      else if (is_time(n)) d[[n]] <- as.POSIXct(d[[n]], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
    attr(d, "description") <- unname(TABLES[t])
    saveRDS(d, file.path(dir, paste0(t, ".rds")))
    ex <- vapply(d, function(v) { v <- v[!is.na(v)]; if (!length(v)) "" else substr(format(v[1]), 1, 40) }, "")
    struct[[t]] <- data.frame(table = t, table_description = unname(TABLES[t]), rows = nrow(d), column = names(d),
                              type = vapply(d, function(v) class(v)[1], ""), description = unname(describe(names(d))),
                              example = unname(ex), stringsAsFactors = FALSE)
    cat(sprintf("%-22s %6d rows %3d columns\n", t, nrow(d), ncol(d)))
  }
  s <- do.call(rbind, struct); rownames(s) <- NULL
  saveRDS(s, file.path(dir, "fms-structure.rds"))
  write.csv(s, file.path(dir, "fms-structure.csv"), row.names = FALSE, fileEncoding = "UTF-8")
} else {
  con <- dbConnect(SQLite(), seed); on.exit(dbDisconnect(con))
  for (f in setdiff(list.files(dir, "\\.rds$", full.names = TRUE), file.path(dir, "fms-structure.rds"))) {
    t <- sub("\\.rds$", "", basename(f)); d <- readRDS(f)
    for (n in names(d)) {
      if (inherits(d[[n]], "Date")) d[[n]] <- format(d[[n]], "%Y-%m-%d")
      else if (inherits(d[[n]], "POSIXct")) d[[n]] <- format(d[[n]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
    attr(d, "description") <- NULL
    if (dbExistsTable(con, t)) { dbExecute(con, sprintf("DELETE FROM %s", t)); dbAppendTable(con, t, d) } else dbWriteTable(con, t, d)
    cat(sprintf("%-22s %6d rows imported\n", t, nrow(d)))
  }
}
