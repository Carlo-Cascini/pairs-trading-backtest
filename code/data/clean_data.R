# Load necessary libraries
library(quantmod)
library(dplyr)
library(zoo) # For na.locf function
library(openxlsx) 

stock_namelist <- c("EWA", "EWK", "EWO", "EWC", "EWQ", "EWG", "EWH",
                    "EWI", "EWJ", "EWM", "EWW", "EWN", "EWS", "EWP",
                    "EWD", "EWL", "EWY", "EZU", "EWU", "EWZ", "EWT",
                    "SPY", "EZA", "EPI", "RSX", "TUR", "KSA", "GREK",
                    "EIS", "ARGT", "THD", "PIN", "NORW", "IEUR", "EEM",
                    "VWO", "AAXJ", "ILF", "AFK", "FEZ", "XLF", "XLK",
                    "XLE", "XLV", "XLY", "XLI", "XLB", "XLU", "IYR",
                    "SMH", "XBI", "VTI", "VOO", "IVV", "QQQ", "IWV",
                    "GLD", "SLV", "PPLT", "PALL", "USO", "BNO", "UNG",
                    "DBO", "DBC", "CORN", "SOYB", "WEAT", "COW", "JJG",
                    "UGA", "OIL", "DBA", "GSG", "COMT", "RJI", "FTGC",
                    "BCI", "SOXX", "FDN", "TAN", "ICLN", "PBW", "IBB",
                    "XBI", "GEX", "PNQI")

begin_date <- as.Date("2010-01-01")
end_date <- as.Date("2024-05-01")  #setta la data di oggi
prices <- xts()

# loop per cercare stock tickers su ogni periodo 
for (stock_index in 1:length(stock_namelist)) {
  stock_data <- try(getSymbols(stock_namelist[stock_index], src = 'yahoo',
                               from = begin_date, to = end_date, auto.assign = FALSE), silent = TRUE)
    if (inherits(stock_data, "xts")) {
    adjusted_prices <- Ad(stock_data)
    prices <- cbind(prices, adjusted_prices)
  }
}

#process data 
colnames(prices) <- stock_namelist
tclass(prices) <- "Date"
prices <- prices[, !duplicated(colnames(prices))]

valid_series <- apply(prices, 2, function(x) all(!is.na(x[index(x) < as.Date("2010-01-01")])))
prices <- prices[, valid_series]

# missing values only con forward fill to avoid look-ahead bias
prices <- na.locf(prices, na.rm = FALSE)

prices <- na.omit(prices)

output_file <- "/Users/carlocascini/Desktop/pairs-trading/code/data/67_cleaned_etfs.xlsx"
write.xlsx(as.data.frame(prices), file = output_file, rowNames = TRUE)
