# From:
# https://datashenanigan.wordpress.com/2016/05/24/a-gentle-introduction-to-finance-using-r-efficient-frontier-and-capm-part-1/



library(tseries)
link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/mult_assets.csv"
df <- data.table(read.csv(link))

df_table <- melt(df)[, .(er = mean(value),
                         sd = sd(value)), by = variable]

er_vals <- seq(from = min(df_table$er), to = max(df_table$er), length.out = 1000)

# find an optimal portfolio for each possible possible expected return 
# (note that the values are explicitly set between the minimum and maximum of the expected returns per asset)
sd_vals <- sapply(er_vals, function(er) {
  op <- portfolio.optim(as.matrix(df), er)
  return(op)
})

