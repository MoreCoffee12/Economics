



# This became onerous to keep in the main mark-up document so I broke it out.

string.symbol = c("CPIAUCSL", "USREC")
string.source = c("FRED", "FRED")
string.description = c(
  "Consumer Price Index for All\nUrban Consumers: All Items",
  "NBER based Recession Indicators"
)
string.label.y = c("Index 1982-1984=100", "+1 or 0")
float.expense.ratio = c(-1.00, -1.00)       
date.series.start = as.Date("1900-01-01")
date.series.end = as.Date("1900-01-01")
df.symbols = data.frame(
  string.symbol,
  string.source,
  string.description,
  string.label.y,
  float.expense.ratio,
  date.series.start,
  date.series.end,
  stringsAsFactors = FALSE
)

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "UNRATE",
      string.source = "FRED",
      string.description = "Civilian Unemployment Rate U-3",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PCEPI",
      string.source = "FRED",
      string.description = "Personal Consumption Expenditures:\nChain-type Price Index",
      string.label.y = "Index 2012=100",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CCSA",
      string.source = "FRED",
      string.description = "Continued Claims (Insured Unemployment)",
      string.label.y = "Number",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CCNSA",
      string.source = "FRED",
      string.description = "Continued Claims (Insured Unemployment, NSA)",
      string.label.y = "Number",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "NPPTTL",
      string.source = "FRED",
      string.description = "Total Nonfarm Private Payroll Employment (ADP)",
      string.label.y = "Thousands",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "U6RATE",
      string.source = "FRED",
      string.description = "Total unemployed + margin + part-time U-6",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PAYNSA",
      string.source = "FRED",
      string.description = "All Employees: Total Nonfarm Payrolls (NSA)",
      string.label.y = "Thousands of Persons",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1939-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TABSHNO",
      string.source = "FRED",
      string.description = "Households and nonprofit\norganizations; total assets, Level",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "HNONWPDPI",
      string.source = "FRED",
      string.description = "Household Net Worth, percent Dispsable Income",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "INDPRO",
      string.source = "FRED",
      string.description = "Industrial Production Index",
      string.label.y = "Index 2012=100",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RRSFS",
      string.source = "FRED",
      string.description = "Real Retail and Food Services Sales",
      string.label.y = "Millions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RSALES",
      string.source = "FRED",
      string.description = "Real Retail Sales (DISCONTINUED)",
      string.label.y = "Millions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "W875RX1",
      string.source = "FRED",
      string.description = "Real personal income\nexcluding current transfer receipts",
      string.label.y = "Billions of Chained 2009 Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RPI",
      string.source = "FRED",
      string.description = "Real personal income",
      string.label.y = "Billions of Chained 2009 Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PCOPPUSDM",
      string.source = "FRED",
      string.description = "Global price of Copper",
      string.label.y = "U.S. Dollars per Metric Ton",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "NOBL",
      string.source = "yahoo",
      string.description = "ProShares S&P 500 Dividend Aristocrats (NOBL)",
      string.label.y = "BATS Real Time Price",
      float.expense.ratio = 0.35,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "SCHD",
      string.source = "yahoo",
      string.description = "Schwab U.S. Dividend Equity ETF",
      string.label.y = "Dollars",
      float.expense.ratio = 0.06,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PFF",
      string.source = "yahoo",
      string.description = "iShares Preferred and Income Securities ETF",
      string.label.y = "BATS Real Time Price",
      float.expense.ratio = 0.35,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "HPI",
      string.source = "yahoo",
      string.description = "John Hancock Preferred Income Fund",
      string.label.y = "BATS Real Time Price",
      float.expense.ratio = 0.35,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GSFTX",
      string.source = "yahoo",
      string.description = "Columbia Dividend Income Fund Institutional Class",
      string.label.y = "Dollars",
      float.expense.ratio = 0.71,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "LFMIX",
      string.source = "yahoo",
      string.description = "LoCorr Macro Strategies Fund Class I",
      string.label.y = "Dollars",
      float.expense.ratio = 2.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
  df.symbols,
  data.frame(
    string.symbol = "LFMCX",
    string.source = "yahoo",
    string.description = "LoCorr Macro Strategies Fund Class C",
    string.label.y = "Dollars",
    float.expense.ratio = 3.00,
    date.series.start = as.Date("1900-01-01"),
    date.series.end = as.Date("1900-01-01")
  )
)

df.symbols <-
  rbind(
  df.symbols,
  data.frame(
    string.symbol = "LFMAX",
    string.source = "yahoo",
    string.description = "LoCorr Macro Strategies Fund Class A",
    string.label.y = "Dollars",
    float.expense.ratio = 2.25,
    date.series.start = as.Date("1900-01-01"),
    date.series.end = as.Date("1900-01-01")
  )
)

df.symbols <-
  rbind(
  df.symbols,
  data.frame(
    string.symbol = "LCSIX",
    string.source = "yahoo",
    string.description = "LoCorr Long/Short Commodity Strategies Fund Class I",
    string.label.y = "Dollars",
    float.expense.ratio = 2.40,
    date.series.start = as.Date("1900-01-01"),
    date.series.end = as.Date("1900-01-01")
  )
)

df.symbols <-
  rbind(
  df.symbols,
  data.frame(
    string.symbol = "BSV",
    string.source = "yahoo",
    string.description = "Vanguard Short-Term Bond Index Fund ETF Shares",
    string.label.y = "Dollars",
    float.expense.ratio = 0.05,
    date.series.start = as.Date("1900-01-01"),
    date.series.end = as.Date("1900-01-01")
  )
)

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "VBIRX",
      string.source = "yahoo",
      string.description = "Vanguard Short-Term Bond Index Fund Admiral Shares",
      string.label.y = "Dollars",
      float.expense.ratio = 0.07,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BIV",
      string.source = "yahoo",
      string.description = "Vanguard Intermediate-term Bond Index Fund ETF Shares",
      string.label.y = "Dollars",
      float.expense.ratio = 0.05,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "VFSUX",
      string.source = "yahoo",
      string.description = "Vanguard Short-Term Investment-Grade Fund Admiral Shares",
      string.label.y = "Dollars",
      float.expense.ratio = 0.10,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "LTUIX",
      string.source = "yahoo",
      string.description = "Thornburg Limited Term U.S. Government Fund Class I",
      string.label.y = "Dollars",
      float.expense.ratio = 0.62,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PTTPX",
      string.source = "yahoo",
      string.description = "PIMCO Total Return Fund Class I-2",
      string.label.y = "Dollars",
      float.expense.ratio = 0.81,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "NERYX",
      string.source = "yahoo",
      string.description = "Loomis Sayles Core Plus Bond Fund Class Y",
      string.label.y = "Dollars",
      float.expense.ratio = 0.48,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "STIGX",
      string.source = "yahoo",
      string.description = "Virtus Seix Core Bond Fund Class I",
      string.label.y = "Dollars",
      float.expense.ratio = 0.50,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "HLGAX",
      string.source = "yahoo",
      string.description = "JPMorgan Government Bond Fund Class I",
      string.label.y = "Dollars",
      float.expense.ratio = 0.48,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FTRGX",
      string.source = "yahoo",
      string.description = "Federated Hermes Total Return Government Bond Fund Institutional Shares",
      string.label.y = "Dollars",
      float.expense.ratio = 0.33,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )



df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "THIIX",
      string.source = "yahoo",
      string.description = "Thornburg Limited Term Income Fund Class I",
      string.label.y = "Dollars",
      float.expense.ratio = 0.49,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
  df.symbols,
  data.frame(
    string.symbol = "PTTRX",
    string.source = "yahoo",
    string.description = "PIMCO Total Return Fund Institutional Class",
    string.label.y = "Dollars",
    float.expense.ratio = 0.71,
    date.series.start = as.Date("1900-01-01"),
    date.series.end = as.Date("1900-01-01")
  )
)



df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BFIGX",
      string.source = "yahoo",
      string.description = "American Funds Inflation Linked Bond Fund Class F-2",
      string.label.y = "Dollars",
      float.expense.ratio = 0.43,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
  df.symbols,
  data.frame(
    string.symbol = "VTWO",
    string.source = "yahoo",
    string.description = "Vanguard Russell 2000 Index Fund ETF Shares",
    string.label.y = "Dollars",
    float.expense.ratio = 0.10,
    date.series.start = as.Date("1900-01-01"),
    date.series.end = as.Date("1900-01-01")
  )
)

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "EIFAX",
      string.source = "yahoo",
      string.description = "Eaton Vance Floating-Rate Advantage Fund Class I",
      string.label.y = "Dollars",
      float.expense.ratio = 1.62,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ASDAX",
      string.source = "yahoo",
      string.description = "AAM/HIMCO Short Duration Fund Class A",
      string.label.y = "Dollars",
      float.expense.ratio = 0.84,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TRBUX",
      string.source = "yahoo",
      string.description = "T. Rowe Price Ultra Short-Term Bond Fund",
      string.label.y = "Dollars",
      float.expense.ratio = 0.32,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PRVIX",
      string.source = "yahoo",
      string.description = "T. Rowe Price Small-Cap Value Fund I Class",
      string.label.y = "Dollars",
      float.expense.ratio = 0.30,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PRWCX",
      string.source = "yahoo",
      string.description = "T. Rowe Price Capital Appreciation Fund",
      string.label.y = "Dollars",
      float.expense.ratio = 0.70,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ADOZX",
      string.source = "yahoo",
      string.description = "Alger Dynamic Opportunities Fund Class Z",
      string.label.y = "Dollars",
      float.expense.ratio = 1.75,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "MERFX",
      string.source = "yahoo",
      string.description = "The Merger Fund Investor Class",
      string.label.y = "Dollars",
      float.expense.ratio = 1.99,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CMNIX",
      string.source = "yahoo",
      string.description = "Calamos Market Neutral Income Fund Institutional Class",
      string.label.y = "Dollars",
      float.expense.ratio = 1.01,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CIHEX",
      string.source = "yahoo",
      string.description = "Calamos Hedged Equity Fund Class I ",
      string.label.y = "Dollars",
      float.expense.ratio = 0.92,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )
























#-----------------------------------------------------------------------------------------
# Data from US Census Bureau
#-----------------------------------------------------------------------------------------

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "IMPCH",
      string.source = "FRED",
      string.description = "U.S. Imports of Goods by Customs\nBasis from China (Monthly, NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "EXPCH",
      string.source = "FRED",
      string.description = "U.S. Exports of Goods by F.A.S.\nBasis to China, Mainland (Monthly, NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "IMPMX",
      string.source = "FRED",
      string.description = "U.S. Imports of Goods by Customs\nBasis from Mexico (Monthly, NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "EXPMX",
      string.source = "FRED",
      string.description = "U.S. Exports of Goods by F.A.S.\nBasis to Mexico (Monthly, NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )



df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "HSN1FNSA",
      string.source = "FRED",
      string.description = "New One Family Houses Sold: United States (Monthly, NSA)",
      string.label.y = "Thousands of Units",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "HNFSUSNSA",
      string.source = "FRED",
      string.description = "New One Family Houses for Sale in the United States (Monthly, NSA)",
      string.label.y = "Thousands of Units",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )























#-----------------------------------------------------------------------------------------
# Data from the "H.8 Assets and Liabilities of Commercial Banks in the United States" table
#-----------------------------------------------------------------------------------------
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BUSLOANS",
      string.source = "FRED",
      string.description = "Commercial and Industrial Loans,\n All Commercial Banks (Monthly, SA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TOTCI",
      string.source = "FRED",
      string.description = "Commercial and Industrial Loans,\n All Commercial Banks (Weekly, SA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BUSLOANSNSA",
      string.source = "FRED",
      string.description = "Commercial and Industrial Loans,\n All Commercial Banks (Monthly, NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "REALLNNSA",
      string.source = "FRED",
      string.description = "Real Estate Loans,\nAll Commercial Banks (Monthly, NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "REALLN",
      string.source = "FRED",
      string.description = "Real Estate Loans,\nAll Commercial Banks (Monthly, SA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RELACBW027NBOG",
      string.source = "FRED",
      string.description = "Real Estate Loans,\nAll Commercial Banks (Weekly, NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RELACBW027SBOG",
      string.source = "FRED",
      string.description = "Real Estate Loans,\nAll Commercial Banks (Weekly, SA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RREACBM027NBOG",
      string.source = "FRED",
      string.description = "Real Estate Loans: Residential Real Estate Loans,\nAll Commercial Banks (Monthly, NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RREACBM027SBOG",
      string.source = "FRED",
      string.description = "Real Estate Loans: Residential Real Estate Loans,\nAll Commercial Banks (Monthly, SA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RREACBW027SBOG",
      string.source = "FRED",
      string.description = "Real Estate Loans: Residential Real Estate Loans,\nAll Commercial Banks (Weekly, SA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RREACBW027NBOG",
      string.source = "FRED",
      string.description = "Real Estate Loans: Residential Real Estate Loans,\nAll Commercial Banks (Weekly, NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "MORTGAGE30US",
      string.source = "FRED",
      string.description = "30-Year Fixed Rate Mortgage Average in the United States",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CONSUMERNSA",
      string.source = "FRED",
      string.description = "Consumer Loans, All Commercial Banks",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TOTLLNSA",
      string.source = "FRED",
      string.description = "Loans and Leases in Bank Credit,\nAll Commercial Banks",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DPSACBW027SBOG",
      string.source = "FRED",
      string.description = "Deposits, All Commercial Banks",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DRCLACBS",
      string.source = "FRED",
      string.description = "Delinquency Rate on Consumer Loans,\nAll Commercial Banks, SA",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TOTCINSA",
      string.source = "FRED",
      string.description = "Commercial and Industrial Loans,\nAll Commercial Banks (Weekly, NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )
















#-----------------------------------------------------------------------------------------
# Data from the "Z.1 Financial Accounts of the United States" table
#-----------------------------------------------------------------------------------------
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "SRPSABSNNCB",
      string.source = "FRED",
      string.description = "Nonfinancial corporate business;\nsecurity repurchase agreements;\nasset, Level (NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ASTLL",
      string.source = "FRED",
      string.description = "All sectors; total loans; liability, Level (NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FBDILNECA",
      string.source = "FRED",
      string.description = "Domestic financial sectors; depository institution\nloans n.e.c.; asset, Level (NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ASOLAL",
      string.source = "FRED",
      string.description = "All sectors; other loans and advances;\nliability, Level (NSA)",
      string.label.y = "Millions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ASTMA",
      string.source = "FRED",
      string.description = "All sectors; total mortgages; asset, Level (NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ASHMA",
      string.source = "FRED",
      string.description = "All sectors; home mortgages; asset, Level (NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ASMRMA",
      string.source = "FRED",
      string.description = "All sectors; multifamily residential\nmortgages; asset, Level (NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ASCMA",
      string.source = "FRED",
      string.description = "All sectors; commercial mortgages;\nasset, Level  (NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ASFMA",
      string.source = "FRED",
      string.description = "All sectors; farm mortgages;\nasset, Level (NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CCLBSHNO",
      string.source = "FRED",
      string.description = "Households and nonprofit organizations;\nconsumer credit; liability, Level (NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FBDSILQ027S",
      string.source = "FRED",
      string.description = "Domestic financial sectors\ndebt securities; liability, Level (NSA)",
      string.label.y = "Millions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FBLL",
      string.source = "FRED",
      string.description = "Domestic financial sectors\nloans; liability, Level (NSA)",
      string.label.y = "Millions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "NCBDBIQ027S",
      string.source = "FRED",
      string.description = "Nonfinancial corporate\nbusiness; debt securities; liability, Level",
      string.label.y = "Millions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DGS10",
      string.source = "FRED",
      string.description = "10-Year Treasury Constant Maturity Rate",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "^TNX",
      string.source = "yahoo",
      string.description = "CBOE Interest Rate 10 Year T No",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CL=F",
      string.source = "yahoo",
      string.description = "Crude Oil Futures",
      string.label.y = "Dollars/bbl",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DGS30",
      string.source = "FRED",
      string.description = "10-Year Treasury Constant Maturity Rate",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DGS1",
      string.source = "FRED",
      string.description = "1-Year Treasury Constant Maturity Rate",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DGS2",
      string.source = "FRED",
      string.description = "2-Year Treasury Constant Maturity Rate",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TB3MS",
      string.source = "FRED",
      string.description = "3-Month Treasury Bill: Secondary Market Rate (Monthly)",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DTB3",
      string.source = "FRED",
      string.description = "3-Month Treasury Bill: Secondary Market Rate (Daily)",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "^IRX",
      string.source = "yahoo",
      string.description = "CBOE 13 WEEK TREASURY BILL",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DCOILWTICO",
      string.source = "FRED",
      string.description = "Crude Oil Prices: West Texas Intermediate (WTI)\nCushing, Oklahoma",
      string.label.y = "Dollars per Barrel (Not Adjusted)",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DCOILBRENTEU",
      string.source = "FRED",
      string.description = "Crude Oil Prices: Brent - Europe",
      string.label.y = "Dollars per Barrel (Not Adjusted)",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "NEWORDER",
      string.source = "FRED",
      string.description = "Manufacturers' New Orders: Nondefense Capital Goods Excluding Aircraft",
      string.label.y = "Millions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ALTSALES",
      string.source = "FRED",
      string.description = "Light Weight Vehicle Sales: Autos and Light Trucks",
      string.label.y = "Millions of Units",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ICSA",
      string.source = "FRED",
      string.description = "Initial Jobless Claims",
      string.label.y = "Number",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "^GSPC",
      string.source = "yahoo",
      string.description = "S&P 500",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FXAIX",
      string.source = "yahoo",
      string.description = "Fidelity 500 Index Fund",
      string.label.y = "Dollars",
      float.expense.ratio = 0.01,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FTIHX",
      string.source = "yahoo",
      string.description = "Fidelity Total\nInternational Index Fund",
      string.label.y = "Dollars",
      float.expense.ratio = 0.06,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "MDIZX",
      string.source = "yahoo",
      string.description = "MFS International Diversification Fund Class R6",
      string.label.y = "Dollars",
      float.expense.ratio = 0.70,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DODIX",
      string.source = "yahoo",
      string.description = "Dodge & Cox Income Fund",
      string.label.y = "Dollars",
      float.expense.ratio = 0.42,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )


# df.symbols <-
#   rbind(
#     df.symbols,
#     data.frame(
#       string.symbol = "^SPX",
#       string.source = "yahoo",
#       string.description = "S&P 500",
#       string.label.y = "Dollars",
#       float.expense.ratio = -1.00,
#       date.series.start = as.Date("1900-01-01") ,
#       date.series.end = as.Date("1900-01-01")
#     )
#   )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "^RLG",
      string.source = "yahoo",
      string.description = "Russell 1000 Growth ETF",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "^DJI",
      string.source = "yahoo",
      string.description = "Dow Jones Industrial Average",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "^STOXX50E",
      string.source = "yahoo",
      string.description = "Euro Stoxx 50",
      string.label.y = "Euros",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "EFA",
      string.source = "yahoo",
      string.description = "iShares MSCI EAFE ETF",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GDP",
      string.source = "FRED",
      string.description = "Gross Domestic Product",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FNDEFX",
      string.source = "FRED",
      string.description = "Federal Government: Nondefense\nConsumption Expenditures and Gross Investment (SA, Annual Rate)",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FDEFX",
      string.source = "FRED",
      string.description = "Federal Government: National Defense\nConsumption Expenditures and Gross Investment (SA, Annual Rate)",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GDPNOW",
      string.source = "FRED",
      string.description = "Fed Atlanta GDPNow",
      string.label.y = "Percent Change at Annual Rate (SA)",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GDPC1",
      string.source = "FRED",
      string.description = "Real Gross Domestic Product",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GDPDEF",
      string.source = "FRED",
      string.description = "Gross Domestic Product: Implicit Price Deflator",
      string.label.y = "Index 2012=100",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "VIG",
      string.source = "yahoo",
      string.description = "Vanguard Dividend Appreciation ETF",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )


#-----------------------------------------------------------------------------------------
# Federal reserve data
#-----------------------------------------------------------------------------------------

# Native units are millions, but this gets converted to billions in the calcAggregateSeries functions.
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "WLRRAL",
      string.source = "FRED",
      string.description = "Liabilities and Capital:\nLiabilities: Reverse Repurchase Agreements:\nWednesday Level (NSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FEDFUNDS",
      string.source = "FRED",
      string.description = "Effective Federal Funds Rate",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GPDI",
      string.source = "FRED",
      string.description = "Gross Private Domestic Investment",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "W790RC1Q027SBEA",
      string.source = "FRED",
      string.description = "Net domestic investment: Private: Domestic busines",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "MZMV",
      string.source = "FRED",
      string.description = " Velocity of MZM Money Stock",
      string.label.y = "Ratio",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "M1",
      string.source = "FRED",
      string.description = "M1 Money Stock",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "M2",
      string.source = "FRED",
      string.description = "M2 Money Stock",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "OPHNFB",
      string.source = "FRED",
      string.description = "Nonfarm Business Sector: Real Output Per Hour of All Persons",
      string.label.y = "Index 2009 = 100",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "IPMAN",
      string.source = "FRED",
      string.description = "Industrial Production: Manufacturing (NAICS)",
      string.label.y = "Index 2012 = 100",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "IWD",
      string.source = "yahoo",
      string.description = "iShares Russell 1000 Value ETF",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GS5",
      string.source = "FRED",
      string.description = "5-Year Treasury Constant Maturity Rate",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PSAVERT",
      string.source = "FRED",
      string.description = "Personal Saving Rate",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "VIXCLS",
      string.source = "FRED",
      string.description = "CBOE Volatility Index",
      string.label.y = "Index",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "VXX",
      string.source = "yahoo",
      string.description = "iPath Series B S&P 500 VIX Short-Term Futures ETN",
      string.label.y = "Index",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "HOUST1F",
      string.source = "FRED",
      string.description = "Privately Owned Housing Starts: 1-Unit Structures",
      string.label.y = "Thousands of units",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GFDEBTN",
      string.source = "FRED",
      string.description = "Federal Debt: Total Public Debt",
      string.label.y = "Millions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "HOUST",
      string.source = "FRED",
      string.description = "Housing Starts: Total: New Privately\nOwned Housing Units Started",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "EXHOSLUSM495S",
      string.source = "FRED",
      string.description = "Existing Home Sales",
      string.label.y = "Number of Units",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "MSPUS",
      string.source = "FRED",
      string.description = "Median Sales Price of\nHouses Sold for the United States (NSA)",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "UMDMNO",
      string.source = "FRED",
      string.description = "Manufacturers' New Orders: Durable Goods (NSA)",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DGORDER",
      string.source = "FRED",
      string.description = "Manufacturers' New Orders: Durable Goods (SA)",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CSUSHPINSA",
      string.source = "FRED",
      string.description = "S&P/Case-Shiller U.S. National Home Price Index (NSA)",
      string.label.y = "Index Jan 2000=100",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1987-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GFDEGDQ188S",
      string.source = "FRED",
      string.description = "Federal Debt: Total Public Debt as\nPercent of Gross Domestic Product",
      string.label.y = "Percent of GDP",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FYFSD",
      string.source = "FRED",
      string.description = "Federal Surplus or Deficit",
      string.label.y = "Millions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FYFSGDA188S",
      string.source = "FRED",
      string.description = "Federal Surplus or Deficit [-] as Percent of Gross Domestic Product",
      string.label.y = "Percent of GDP",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GDX",
      string.source = "yahoo",
      string.description = "VanEck Vectors Gold Miners ETF",
      string.label.y = "Dollars",
      float.expense.ratio = 0.53,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "XLE",
      string.source = "yahoo",
      string.description = "Energy Select Sector SPDR Fund",
      string.label.y = "Dollars",
      float.expense.ratio = 0.13,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )



df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GSG",
      string.source = "yahoo",
      string.description = "iShares S&P GSCI Commodity-Indexed Trust",
      string.label.y = "Dollars",
      float.expense.ratio = 0.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )


# This data comes in as millions of dollars, but is converted to billions later so it can be compared with other fed data series.
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "WALCL",
      string.source = "FRED",
      string.description = "All Federal Reserve Banks: Total Assets",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "OUTMS",
      string.source = "FRED",
      string.description = "Manufacturing Sector: Real Output",
      string.label.y = "Index 2009=100",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "MANEMP",
      string.source = "FRED",
      string.description = "All Employees: Manufacturing",
      string.label.y = "Thousands of Persons",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PRS30006163",
      string.source = "FRED",
      string.description = "Manufacturing Sector: Real Output Per Person",
      string.label.y = "Index 2009=100",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BAMLC0A3CA",
      string.source = "FRED",
      string.description = "ICE BofAML US Corporate A Option-Adjusted Spread",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "AAA",
      string.source = "FRED",
      string.description = "Moody's Seasoned Aaa Corporate Bond Yield",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "SOFR",
      string.source = "FRED",
      string.description = "Secured Overnight Financing Rate",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "SOFRVOL",
      string.source = "FRED",
      string.description = "Secured Overnight Financing Volume",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "SOFR99",
      string.source = "FRED",
      string.description = "Secured Overnight Financing Rate:\n99th Percentile",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "SOFR75",
      string.source = "FRED",
      string.description = "Secured Overnight Financing Rate:\n75th Percentile",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "SOFR25",
      string.source = "FRED",
      string.description = "Secured Overnight Financing Rate:\n25th Percentile",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "SOFR1",
      string.source = "FRED",
      string.description = "Secured Overnight Financing Rate:\n1st Percentile",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "OBFR",
      string.source = "FRED",
      string.description = "Overnight Bank Funding Rate",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "OBFR99",
      string.source = "FRED",
      string.description = "Overnight Bank Funding Rate:\n99th Percentile",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "OBFR75",
      string.source = "FRED",
      string.description = "Overnight Bank Funding Rate:\n75th Percentile",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "OBFR25",
      string.source = "FRED",
      string.description = "Overnight Bank Funding Rate:\n25th Percentile",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "OBFR1",
      string.source = "FRED",
      string.description = "Overnight Bank Funding Rate:\n1st Percentile",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RPONTSYD",
      string.source = "FRED",
      string.description = "Overnight Repurchase Agreements:\nTreasury Securities Purchased by\nthe Federal Reserve in the Temporary\nOpen Market Operations ",
      string.label.y = "Billions of US Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "IOER",
      string.source = "FRED",
      string.description = "Interest Rate on Excess Reserves",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "WRESBAL",
      string.source = "FRED",
      string.description = "Reserve Balances with Federal Reserve Banks",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "EXCSRESNW",
      string.source = "FRED",
      string.description = "Excess Reserves of Depository Institutions",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

# Billions are easier to read than millions so this series gets converted to billions of euros
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ECBASSETS",
      string.source = "FRED",
      string.description = "Central Bank Assets for Euro Area (11-19 Countries)",
      string.label.y = "Billions of Euros",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

# This one also gets converted to billions
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "EUNNGDP",
      string.source = "FRED",
      string.description = " Gross Domestic Product (Euro/ECU series) for Euro Area (19 Countries)",
      string.label.y = "Billions of Euros",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CEU0600000007",
      string.source = "FRED",
      string.description = "Average Weekly Hours of Production \n and Nonsupervisory Employees: Goods-Producing",
      string.label.y = "Hours",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

# TODO - replace this data
# df.symbols <-
#   rbind(
#     df.symbols,
#     data.frame(
#       string.symbol = "USD1MTD156N",
#       string.source = "FRED",
#       string.description = "1-Month London Interbank Offered Rate (LIBOR), based on U.S. Dollar",
#       string.label.y = "Percent",
#       float.expense.ratio = -1.00,
#       date.series.start = as.Date("1900-01-01"),
#       date.series.end = as.Date("1900-01-01")
#     )
#   )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CURRENCY",
      string.source = "FRED",
      string.description = "Currency Component of M1 (Seasonally Adjusted)",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "WCURRNS",
      string.source = "FRED",
      string.description = "Currency Component of M1",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BOGMBASE",
      string.source = "FRED",
      string.description = "Monetary Base; Total",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PRS88003193",
      string.source = "FRED",
      string.description = "Nonfinancial Corporations Sector: Unit Profits",
      string.label.y = "Index 2012 = 100",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1947-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PPIACO",
      string.source = "FRED",
      string.description = "Producer Price Index for All Commodities",
      string.label.y = "Index 1982 = 100",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1913-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PCUOMFGOMFG",
      string.source = "FRED",
      string.description = "Producer Price Index by Industry: Total Manufacturing Industries",
      string.label.y = "Index 1982 = 100",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1984-12-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "POPTHM",
      string.source = "FRED",
      string.description = "Population (U.S.)",
      string.label.y = "Thousands",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1959-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "POPTHM",
      string.source = "FRED",
      string.description = "Population (U.S.)",
      string.label.y = "Thousands",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1959-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CLF16OV",
      string.source = "FRED",
      string.description = "Civilian Labor Force Level, SA ",
      string.label.y = "Thousands",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1948-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "LNU01000000",
      string.source = "FRED",
      string.description = "Civilian Labor Force Level, NSA",
      string.label.y = "Thousands",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1948-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "LNU03000000",
      string.source = "FRED",
      string.description = "Unemployment Level (NSA)",
      string.label.y = "Thousands",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1959-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "UNEMPLOY",
      string.source = "FRED",
      string.description = "Unemployment Level, seasonally adjusted",
      string.label.y = "Thousands",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1948-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RSAFS",
      string.source = "FRED",
      string.description = "Advance Retail Sales: Retail and Food Services",
      string.label.y = "Thousands",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1992-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FRGSHPUSM649NCIS",
      string.source = "FRED",
      string.description = "Cass Freight Index: Shipments",
      string.label.y = "Index Jan 1990 = 1",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1999-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BOPGTB",
      string.source = "FRED",
      string.description = "Trade Balance: Goods,\nBalance of Payments Basis (SA)",
      string.label.y = "Millions of dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1999-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TERMCBPER24NS",
      string.source = "FRED",
      string.description = "Finance Rate on Personal Loans at\nCommercial Banks, 24 Month Loan",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1972-02-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "A065RC1A027NBEA",
      string.source = "FRED",
      string.description = "Personal income (NSA)",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1929-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PI",
      string.source = "FRED",
      string.description = "Personal income (SA)",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1959-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PCE",
      string.source = "FRED",
      string.description = "Personal Consumption\nExpenditures (SA)",
      string.label.y = "Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1959-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "A053RC1Q027SBEA",
      string.source = "FRED",
      string.description = "National income: Corporate\nprofits before tax (without IVA and CCAdj)",
      string.label.y = "Billions of dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1947-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CPROFIT",
      string.source = "FRED",
      string.description = "Corporate Profits with Inventory\nValuation Adjustment (IVA) and\nCapital Consumption Adjustment (CCAdj)",
      string.label.y = "Billions of dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1947-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "SPY",
      string.source = "yahoo",
      string.description = "SPDR S&P 500 ETF",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1993-02-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "MDY",
      string.source = "yahoo",
      string.description = "SPDR S&P MidCap 400 ETF",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1995-09-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "EES",
      string.source = "yahoo",
      string.description = "WisdomTree US SmallCap Earnings ETF",
      string.label.y = "Dollars",
      float.expense.ratio = 0.38,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "IJR",
      string.source = "yahoo",
      string.description = "iShares Core S&P Small-Cap ETF",
      string.label.y = "Dollars",
      float.expense.ratio = 0.05,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )



df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "VGSTX",
      string.source = "yahoo",
      string.description = "Vanguard STAR Inv",
      string.label.y = "Dollars",
      float.expense.ratio = 0.32,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "VFINX",
      string.source = "yahoo",
      string.description = "Vanguard 500 Index Investor",
      string.label.y = "Dollars",
      float.expense.ratio = 0.14,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "VOE",
      string.source = "yahoo",
      string.description = "Vanguard Mid-Cap Value ETF",
      string.label.y = "Dollars",
      float.expense.ratio = 0.07,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "VOT",
      string.source = "yahoo",
      string.description = "Vanguard Mid-Cap Growth ETF",
      string.label.y = "Dollars",
      float.expense.ratio = 0.07,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TMFGX",
      string.source = "yahoo",
      string.description = "Motley Fool Great America Investor",
      string.label.y = "Dollars",
      float.expense.ratio = 1.16,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "IWM",
      string.source = "yahoo",
      string.description = "iShares Russell 2000",
      string.label.y = "Dollars",
      float.expense.ratio = 0.19,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ONEQ",
      string.source = "yahoo",
      string.description = "Fidelity NASDAQ Composite Index Tracking Stock Fund",
      string.label.y = "Dollars",
      float.expense.ratio = 0.21,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FSMAX",
      string.source = "yahoo",
      string.description = "Fidelity Extended Market Index Fund ",
      string.label.y = "Dollars",
      float.expense.ratio = 0.03,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FXNAX",
      string.source = "yahoo",
      string.description = "Fidelity U.S. Bond Index Fund",
      string.label.y = "Dollars",
      float.expense.ratio = 0.03,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "HAINX",
      string.source = "yahoo",
      string.description = "Harbor International Institutional",
      string.label.y = "Dollars",
      float.expense.ratio = 0.81,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "HNACX",
      string.source = "yahoo",
      string.description = "Harbor Capital Appreciation\nFund Retirement Class",
      string.label.y = "Dollars",
      float.expense.ratio = 0.59,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "VEU",
      string.source = "yahoo",
      string.description = "Vanguard FTSE All-Wld ex-US ETF",
      string.label.y = "Dollars",
      float.expense.ratio = 0.11,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "VEIRX",
      string.source = "yahoo",
      string.description = "Vanguard Equity-Income\nFund Admiral Shares",
      string.label.y = "Dollars",
      float.expense.ratio = 0.19,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BIL",
      string.source = "yahoo",
      string.description = "SPDR Blmbg Barclays 1-3 Mth T-Bill ETF",
      string.label.y = "Dollars",
      float.expense.ratio = 0.14,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "IVOO",
      string.source = "yahoo",
      string.description = "Vanguard S&P Mid-Cap 400 ETF",
      string.label.y = "Dollars",
      float.expense.ratio = 0.15,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "VO",
      string.source = "yahoo",
      string.description = "Vanguard Mid-Cap ETF",
      string.label.y = "Dollars",
      float.expense.ratio = 0.05,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CZA",
      string.source = "yahoo",
      string.description = "Invesco Zacks Mid-Cap ETF",
      string.label.y = "Dollars",
      float.expense.ratio = 0.15,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "VYM",
      string.source = "yahoo",
      string.description = "Vanguard High Dividend Yield ETF",
      string.label.y = "Dollars",
      float.expense.ratio = 0.08,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ACWI",
      string.source = "yahoo",
      string.description = "iShares MSCI ACWI ETF",
      string.label.y = "Dollars",
      float.expense.ratio = 0.32,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "SLY",
      string.source = "yahoo",
      string.description = "SPDR S&P 600 Small Cap",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1995-09-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "QQQ",
      string.source = "yahoo",
      string.description = "Invesco QQQ Trust",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1999-04-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "HYMB",
      string.source = "yahoo",
      string.description = "SPDR Nuveen S&P High Yield Municipal Bond ETF",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("2011-04-18") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

#-----------------------------------------------------------------------------------------
# Company specific data
#-----------------------------------------------------------------------------------------


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GOLD",
      string.source = "yahoo",
      string.description = "Barrick Gold Corporation",
      string.label.y = "Dollars",
      float.expense.ratio = 0.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BKR",
      string.source = "yahoo",
      string.description = "Baker Hughes Company",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1800-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "SLB",
      string.source = "yahoo",
      string.description = "Schlumberger Limited",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1800-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "HAL",
      string.source = "yahoo",
      string.description = "Halliburton Company",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1800-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "IP",
      string.source = "yahoo",
      string.description = "International Paper Company",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1800-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )


df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PKG",
      string.source = "yahoo",
      string.description = "Packaging Corporation of America",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1800-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "UPS",
      string.source = "yahoo",
      string.description = "United Parcel Service, Inc.",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1800-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FDX",
      string.source = "yahoo",
      string.description = "FedEx Corporation",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1800-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "T",
      string.source = "yahoo",
      string.description = "AT&T",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1800-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "VZ",
      string.source = "yahoo",
      string.description = "Verizon",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1800-01-01") ,
      date.series.end = as.Date("1900-01-01")
    )
  )

