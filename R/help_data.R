# Help content: the FAQ and the glossary. English text here; translations live in i18n/*.json.

FAQ <- list(
  "Getting started" = list(
    c(q = "What is FMS?",
      a = "FMS is a fund management platform. It brings together fund information, NAV history, investor holdings, trading, back office and reporting in one place."),
    c(q = "Is the data real?",
      a = "No. All funds, prices, investors and market data in this demo are simulated. They are only meant to show how the platform works."),
    c(q = "How do I create an account?",
      a = "Open the Sign up tab on the sign-in screen and fill in the form. An administrator reviews each new account and activates it with the right role."),
    c(q = "What can each role do?",
      a = "Investors and introducers see funds, markets, reports and their own portfolio. Fund traders also trade and price. Fund managers add management, transactions, dividends and commissions. The super administrator sees everything, including users and supervision."),
    c(q = "How do I change the language or theme?",
      a = "Click the options icon at the top right. You can switch between English, Vietnamese, French and Chinese, and between the dark and light themes.")),
  "Funds and NAV" = list(
    c(q = "What is the NAV?",
      a = "The net asset value is the value of a fund's assets minus its liabilities, divided by the number of units. It is the price at which units are subscribed and redeemed."),
    c(q = "How often is the NAV updated?",
      a = "Each fund has its own valuation frequency, shown on its detail page. Staff manage valuations on the NAV & Pricing page."),
    c(q = "How is fund performance calculated?",
      a = "Performance is the change in NAV per unit over the period, with dividends taken into account. The YTD, 1-year and other periods all start from the last NAV before the period."),
    c(q = "What is the benchmark on the performance chart?",
      a = "It is the index the fund is compared with. By default it is the VN-Index for Vietnam funds and the S&P 500 for global funds. You can choose another one from the list above the chart."),
    c(q = "How do I compare several funds?",
      a = "Open Funds, then Compare funds. Pick the funds and the period to see their performance and key figures side by side.")),
  "Investing" = list(
    c(q = "How do I subscribe to a fund?",
      a = "Open My Portfolio, then the New order tab. Choose the fund, select Subscribe and enter an amount. The order is dealt at the NAV of the next dealing day."),
    c(q = "How do I redeem my units?",
      a = "In the New order tab of My Portfolio, select Redeem and enter a number of units. The cash is paid after settlement, at the NAV of the dealing day. A pending order can still be cancelled."),
    c(q = "Why is my order still pending?",
      a = "Orders are processed on the fund's next dealing day and then checked by the back office. Until then the status stays pending. You can follow it in My Operations."),
    c(q = "Where can I see my holdings and dividends?",
      a = "Open the account menu at the top right. My Funds shows your holdings, My Operations your orders, and My Dividends every dividend you received."),
    c(q = "Are there fees?",
      a = "Each fund lists its management fee, performance fee and ongoing charges on its detail page. These fees are already taken into account in the published NAV.")),
  "Reports and data" = list(
    c(q = "How do I download a report?",
      a = "Open Reports, choose the report and its options, then click Download. Click View to open it in a new browser tab instead."),
    c(q = "What does the file name under a table mean?",
      a = "It names the data file in data/rds that the table reads. Update that file to change what the table shows."),
    c(q = "When is market data updated?",
      a = "The Markets page shows closing levels and the time of the last update at the top right of the movers cards.")),
  "Account and security" = list(
    c(q = "How do I change my password?",
      a = "Open My Profile from the account menu at the top right and use the password section."),
    c(q = "I forgot my password. What should I do?",
      a = "Contact your administrator. They can reset it from the Users page."),
    c(q = "Who can see my personal information?",
      a = "Only you and authorised staff. Every change made by staff is recorded in the audit log."))
)

GLOSSARY <- list(
  c(term = "AUM",
    def = "Assets under management: the total market value of everything a fund holds for its investors."),
  c(term = "Benchmark",
    def = "An index used to measure a fund's performance, such as the VN-Index or the S&P 500."),
  c(term = "Cut-off time",
    def = "The latest time an order can be placed to be dealt on the next dealing day."),
  c(term = "Dealing day",
    def = "A day on which the fund accepts subscriptions and redemptions at that day's NAV."),
  c(term = "Distribution",
    def = "A payment of income from the fund to its investors, also called a dividend."),
  c(term = "Drawdown",
    def = "The fall from a peak NAV to a later low. The maximum drawdown is the largest such fall over a period."),
  c(term = "High-water mark",
    def = "The highest NAV on which a performance fee was paid. A new fee is due only above it."),
  c(term = "Introducer",
    def = "A partner who brings investors to the funds and earns a commission on their investments."),
  c(term = "ISIN",
    def = "International Securities Identification Number: the 12-character code that identifies a fund share class."),
  c(term = "Management fee",
    def = "The yearly fee paid to the fund manager, as a percentage of the fund's assets."),
  c(term = "NAV",
    def = "Net asset value: the fund's assets minus its liabilities. NAV per unit divides it by the units in issue."),
  c(term = "Ongoing charges",
    def = "All the yearly costs of running a fund, shown as a percentage of its assets."),
  c(term = "Performance fee",
    def = "A fee paid to the manager when the fund beats its target, usually above a high-water mark."),
  c(term = "Rebalancing",
    def = "Bringing a portfolio's holdings back to their target weights."),
  c(term = "Redemption",
    def = "Selling units back to the fund at the NAV of the dealing day."),
  c(term = "Settlement",
    def = "The step that completes an order, when cash and units actually change hands."),
  c(term = "Sharpe ratio",
    def = "Return above the risk-free rate divided by volatility. Higher means more return for each unit of risk."),
  c(term = "Subscription",
    def = "Buying new units of a fund at the NAV of the dealing day."),
  c(term = "Unit",
    def = "A share of a fund. Each unit is worth the NAV per unit."),
  c(term = "Volatility",
    def = "How much returns move up and down, measured as the annualised standard deviation."),
  c(term = "Withholding tax",
    def = "Tax deducted at source from a dividend before it is paid. FMS applies 5%."),
  c(term = "YTD",
    def = "Year to date: the change from the last NAV of the previous year until today.")
)

# The database tables shown on Help > Data table structure, with what each one holds (same text as tools/export-rds.R).
DATA_TABLES <- c(
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

# Help > Contact: the details shown on the page. Demo values; change them here.
CONTACT <- list(
  company = "NXW Next Wealth",
  email = "support@fms.io.vn",
  address = "Ho Chi Minh City, Vietnam",
  hours = "Monday to Friday, 8:30 to 17:30 (Vietnam time)")

# Topics offered on the Request information form.
REQUEST_TOPICS <- c("General question", "Fund information", "Investing and orders", "My account", "Technical problem", "Other")

# Help > Documents: the user guide, one section per part of the app.
USER_GUIDE <- list(
  c(h = "Signing in", p = "Sign in with your e-mail and password. New users sign up on the same screen; an administrator approves each account and gives it a role. The options icon at the top right changes the language and the light or dark theme."),
  c(h = "Dashboard", p = "The dashboard sums up the funds you can see: assets under management, the best performers over several periods and the latest NAV of each fund."),
  c(h = "Funds", p = "Fund list shows every fund with its NAV, performance and components. Click a fund to open its detail page: overview, performance against a benchmark you choose, composition, documents and terms. Compare funds puts several funds side by side, and the Documents library holds every fund document."),
  c(h = "Markets", p = "Markets shows closing levels for indices, commodities, forex, rates and crypto, the top gainers and losers of the day, and a chart of any instrument. FX rates lists the rates used to convert amounts to USD."),
  c(h = "Investing", p = "Investors place subscription and redemption orders from My Portfolio, in the New order tab. Orders are dealt at the NAV of the next dealing day once the back office approves them. My Funds, My Operations and My Dividends in the account menu show holdings, orders and dividends."),
  c(h = "Operations and risk", p = "Staff manage the funds from the Operations menu: management, trading, back office, transactions, NAV and pricing, dividends and commissions. The Risk menu covers exposure and compliance checks. Each role sees only the pages it can use."),
  c(h = "Reports and calendar", p = "Reports offers each report as a CSV download or as a page to view and print. The calendar shows dividends, document publications, rebalancing days, month-end valuations and orders waiting to deal."),
  c(h = "Help", p = "Help holds the FAQ, the glossary, the structure of the data tables, these documents and the contact page, where you can send a request for information."))
