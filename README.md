# FMS in R Shiny

The R Shiny version of FMS, the Funds Management System. It has the same pages, roles, logins and data as the Node version in the parent folder.

## Run

You need R 4.1 or newer and these packages:

```r
install.packages(c("shiny", "bslib", "DBI", "RSQLite", "plotly", "sodium", "jsonlite", "openssl"))
# sodium needs the libsodium system library (apt install libsodium-dev, brew install libsodium)
```

Then, from this folder:

```bash
R -e 'shiny::runApp(port = 3838)'     # open http://localhost:3838
```

On first start, the app copies `data/fms-seed.db` to `data/fms.db` and works on that copy. To reset the demo, delete `data/fms.db`. To keep the database somewhere else, set `FMS_DB=/path/fms.db`.

To deploy to shinyapps.io, run this from this folder (app names need at least 4 characters, so give one explicitly):

```r
rsconnect::deployApp(appName = "fundms")
```

`.rscignore` keeps the screenshots and tools out of the upload. If the app misbehaves after deploying, `rsconnect::showLogs(appName = "fundms")` prints the server log: the app writes its versions and database path at startup, and any server error, there. It also works on Posit Connect and Shiny Server. On shinyapps.io the working copy of the database resets whenever the app restarts.

## Demo logins

On the sign-in screen, click any demo account.

| Role | Email | Password |
|---|---|---|
| Super admin | superadmin@fms.io.vn | superadmin |
| Fund manager | fund.manager@fms.io.vn | fund |
| Fund trader | fund.trader@fms.io.vn | trader |
| Investor | demo1.investor@fms.io.vn (demo2, demo3) | demo1 (demo2, demo3) |
| Introducer | demo1.introducer@fms.io.vn (demo2) | introducer |

Passwords are stored as libsodium (scrypt) hashes. Hashes made by the Node version still work, and each one is upgraded at that user's next sign-in.

## Options: mode and language

The sliders icon in the top bar (and on the sign-in screen) opens Options: dark or light mode, and the language: English (default), Tiếng Việt or Français. Each user's choice is saved on their account and applied at their next sign-in, on any device. A choice made on the sign-in screen is kept for that account.

Translations live in `i18n/vi.json` and `i18n/fr.json`, keyed by the English text. To translate a new label, add its English text and translation to both files. Fund names, objectives and strategies are fund data and stay in English.

## Layout

- `app.R`: theme, page shell (top bar with the horizontal menu, search, notifications, light/dark toggle and account menu) and the sign-in and sign-up screen.
- `R/db.R`: SQLite connection. `R/auth.R`: password hashing.
- `R/domain.R`: business rules. Covers fund metrics (YTD, rolling 12/24/36 months, volatility, drawdown, Sharpe), holdings at average cost, composition, compliance checks, notifications, and every action (orders, approvals, trades, dividends, users).
- `R/i18n.R`: translation (`tr()` for sentences, and every `renderUI()` translates its labels).
- `R/shell.R`: Options menu, horizontal menu (Operations and Admin are dropdowns), search, notifications, sign-in and sign-out.
- `R/pages_funds.R`: Dashboard (Best Performers and Fund List), Funds, and the fund page with Overview, Performance, Composition and Resources tabs.
- `R/pages_account.R`: My Profile and the personal room. Investors see portfolio, transactions, dividends and new orders. Introducers see clients and commissions. Staff see their activity.
- `R/pages_ops.R`: Markets, Management, Trading, Back Office, Supervision and Users.
- `www/fms.css`, `www/fms.js`: navy and gold styling, page links, row buttons, and the profile photo picker (resized to 256 px in the browser before it is sent).

## Rebuilding the seed data

The seed comes from the shared generator in `../core/seed.js`, so both versions show the same numbers:

```bash
cd ..            # the fms folder
node shiny/tools/export-seed.js
cd shiny && Rscript tools/rehash.R
```
