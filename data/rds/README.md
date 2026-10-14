# FMS demo data as RDS files

One `.rds` file per database table, with the demo data the website ships with.
`fms-structure.rds` (and `fms-structure.csv`) lists every table and column with its type, meaning and an example.

| File | Rows | What it holds |
|---|---|---|
| fms_users.rds | 10 | Accounts, roles, profile, status, fund access |
| fms_sessions.rds | 0 | Remember-me sign-in tokens |
| fms_fund_list.rds | 10 | The funds |
| fms_funds_specs.rds | 10 | Legal and dealing terms, fees, limits |
| fms_funds_prices.rds | 9,820 | Daily NAV per fund |
| fms_funds_intraday.rds | 794 | Today's iNAV every 5 minutes |
| fms_fund_positions.rds | 100 | Futures held by each fund |
| fms_fund_documents.rds | 70 | Factsheets, KID, prospectus, reports |
| fms_benchmark.rds | 9,820 | Benchmark index closes |
| fms_market_list.rds | 32 | Instruments on the Markets page |
| fms_market_prices.rds | 23,104 | Daily closes for those instruments |
| fms_fx_rates.rds | 1,964 | FX rates used for USD totals |
| fms_transactions.rds | 25 | Subscriptions and redemptions |
| fms_trade_orders.rds | 37 | Trader orders and fills |
| fms_dividends.rds | 10 | Dividends declared |
| fms_commissions.rds | 469 | Introducer commissions |
| fms_audit_log.rds | 9 | Activity log |
| fms_requests.rds | 0 | Requests sent from Help > Contact |

Keys: `fund_id` links to `fms_fund_list`; `user_id`, `investor_id`, `introducer_id`, `trader_id`, `processed_by` and `approved_by` link to `fms_users`.
Dates are `Date`, timestamps are `POSIXct` in UTC.

## Use them

```r
setwd("path/to/fms-shiny")
funds  <- readRDS("data/rds/fms_fund_list.rds")
prices <- readRDS("data/rds/fms_funds_prices.rds")
struct <- readRDS("data/rds/fms-structure.rds")   # View(struct)

# change data, save it, then load it into the website's database
funds$fund_name[1] <- "My new fund name"
saveRDS(funds, "data/rds/fms_fund_list.rds")
system("Rscript tools/export-rds.R import")       # rebuilds data/fms-seed.db
# delete data/fms.db (the running copy) so the app starts again from the new seed
```

Keep the column names and types as they are; you can add or remove rows freely.
`Rscript tools/export-rds.R` (no argument) exports the current seed database back to these files.
