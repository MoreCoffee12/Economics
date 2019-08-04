library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Economic indicators"),
  sidebarPanel(
    radioButtons("indicator", "Data series:",
                 c("Consumer Price Index (FRED)" = 1,
                   "Unemployment (FRED)" = 2,
                   "Industrial Production (FRED)" = 3,
                   "Real Retail Sales (FRED)" = 4,
                   "Real personal income (FRED)" = 5,
                   "Gloabal copper prices (FRED)" = 6,
                   "Commercial and Industrial Loans (FRED)" = 7,
                   "YoY Growth Commerical and Industrial..." = 8,
                   "Nonfinancial corporate business (FRED)" = 9,
                   "Nonfinancial corporate business YoY" = 10,
                   "Real Estate Loans (FRED)" = 11,
                   "Consumer Loans (FRED)" = 12,
                   "Total Loans" = 13,
                   "YoY Total Loans" = 14,
                   "10 year bond yield (FRED)" = 15,
                   "1 year bond yield (FRED)" = 16,
                   "10 year minus 1 year" = 17,
                   "Crude, WTI (FRED)" = 18,
                   "Crude, WTI (FRED) and Bently" = 19,
                   "Manufacturer New Orders (FRED)" = 20,
                   "AA Commercial Paper (FRED)" = 21,
                   "AA Commercial Paper 3 to 1-Month Curve (FRED)"=22))
),
  
  mainPanel(
    textOutput("caption"),
    plotOutput('Employment', width = "100%"),
    width = 
  )
))
