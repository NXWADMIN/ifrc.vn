# Adds simulated Vietnam and international stocks to the Markets data: two new asset classes,
# "Vietnam Stocks" and "International Stocks", in fms_market_list and fms_market_prices.
# Each stock follows its home index (VN-Index or S&P 500) with its own beta and noise, on the same dates.
# Run from the shiny folder:  Rscript tools/add-stocks.R   (safe to re-run; it replaces the stocks it added).
# Re-run it after export-seed.js, which rebuilds data/fms-seed.db without them.
# Then delete data/fms.db locally so the app copies the new seed on its next start.
library(DBI)

stocks <- data.frame(
  symbol = c("VCB", "VIC", "VHM", "HPG", "FPT", "VNM", "MWG", "TCB", "MSN", "GAS",
             "AAPL", "MSFT", "NVDA", "AMZN", "GOOGL", "META", "TSLA", "ASML", "NESN", "7203"),
  name = c("Vietcombank", "Vingroup", "Vinhomes", "Hoa Phat Group", "FPT Corporation", "Vinamilk", "Mobile World", "Techcombank", "Masan Group", "PV Gas",
           "Apple", "Microsoft", "NVIDIA", "Amazon", "Alphabet", "Meta Platforms", "Tesla", "ASML Holding", "Nestle", "Toyota Motor"),
  asset_class = rep(c("Vietnam Stocks", "International Stocks"), each = 10),
  region = c(rep("Vietnam", 10), rep("United States", 7), "Netherlands", "Switzerland", "Japan"),
  quote_unit = c(rep("VND", 10), rep("USD", 7), "EUR", "CHF", "JPY"),
  decimals = c(rep(0L, 10), rep(2L, 9), 0L),
  last = c(64500, 46200, 41800, 27350, 128400, 66900, 61200, 24850, 78300, 70100,
           231.4, 512.8, 178.6, 219.7, 191.2, 702.5, 286.9, 712.4, 84.6, 2765),
  beta = c(0.9, 1.3, 1.4, 1.2, 1.0, 0.6, 1.1, 1.2, 1.0, 0.7,
           1.1, 1.0, 1.8, 1.2, 1.1, 1.3, 2.0, 1.3, 0.5, 0.8),
  noise = c(0.012, 0.022, 0.024, 0.018, 0.016, 0.011, 0.020, 0.017, 0.018, 0.013,
            0.012, 0.011, 0.025, 0.016, 0.015, 0.019, 0.032, 0.018, 0.009, 0.013),
  index = rep(c("VNINDEX", "SPX"), each = 10),
  stringsAsFactors = FALSE)

classes <- c("Indices", "Vietnam Stocks", "International Stocks", "Commodities", "Forex", "Rates", "Crypto")
simulate <- function(con) {
  # Widen the asset-class check so the new classes are allowed (SQLite cannot alter a CHECK in place).
  ddl <- dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE name = 'fms_market_list'")$sql
  if (!grepl("Vietnam Stocks", ddl, fixed = TRUE)) {
    dbExecute(con, "PRAGMA foreign_keys = OFF")
    dbExecute(con, sub("CHECK \\(asset_class IN \\([^)]*\\)\\)", sprintf("CHECK (asset_class IN (%s))", paste0("'", classes, "'", collapse = ",")),
                       sub("fms_market_list", "fms_market_list_new", ddl)))
    dbExecute(con, "INSERT INTO fms_market_list_new SELECT * FROM fms_market_list")
    dbExecute(con, "DROP TABLE fms_market_list")
    dbExecute(con, "ALTER TABLE fms_market_list_new RENAME TO fms_market_list")
  }
  ph <- paste(rep("?", nrow(stocks)), collapse = ",")
  dbExecute(con, sprintf("DELETE FROM fms_market_prices WHERE symbol IN (%s)", ph), params = as.list(stocks$symbol))
  dbExecute(con, sprintf("DELETE FROM fms_market_list WHERE symbol IN (%s)", ph), params = as.list(stocks$symbol))
  dbAppendTable(con, "fms_market_list", stocks[, c("symbol", "name", "asset_class", "region", "quote_unit", "decimals")])
  set.seed(20261011)
  for (i in seq_len(nrow(stocks))) {
    s <- stocks[i, ]
    ix <- dbGetQuery(con, "SELECT price_date, close FROM fms_market_prices WHERE symbol = ? ORDER BY price_date", params = list(s$index))
    r <- s$beta * diff(log(ix$close)) + rnorm(nrow(ix) - 1, 0, s$noise)
    r <- r - mean(r) + runif(1, -0.25, 0.8) / length(r)   # total change over the history: -25% to +80% in log terms
    path <- exp(c(0, cumsum(r))); close <- s$last * path / path[length(path)]   # ends at the stock's last price
    if (s$decimals == 0) close <- round(close / 50) * 50 else close <- round(close, s$decimals)
    if (s$quote_unit == "JPY") close <- round(close)
    dbAppendTable(con, "fms_market_prices", data.frame(symbol = s$symbol, price_date = ix$price_date, close = close))
  }
}

seed <- "data/fms-seed.db"
con <- dbConnect(RSQLite::SQLite(), seed)
invisible(dbWithTransaction(con, simulate(con)))
invisible(dbExecute(con, "VACUUM"))
cat(sprintf("%s: %d market instruments, %d prices\n", seed,
            dbGetQuery(con, "SELECT COUNT(*) n FROM fms_market_list")$n, dbGetQuery(con, "SELECT COUNT(*) n FROM fms_market_prices")$n))

# Keep the data/rds copies in step (they are the local reference tables; not published).
if (dir.exists("data/rds")) {
  l <- dbGetQuery(con, "SELECT * FROM fms_market_list"); l$decimals <- as.integer(l$decimals)
  p <- dbGetQuery(con, "SELECT * FROM fms_market_prices ORDER BY symbol, price_date"); p$price_date <- as.Date(p$price_date)
  for (x in list(list("fms_market_list", l), list("fms_market_prices", p))) {
    f <- file.path("data/rds", paste0(x[[1]], ".rds"))
    if (file.exists(f)) attr(x[[2]], "description") <- attr(readRDS(f), "description")
    saveRDS(x[[2]], f)
  }
}
dbDisconnect(con)
