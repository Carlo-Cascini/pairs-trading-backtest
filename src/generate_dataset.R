generate_dataset <- function(stock_namelist, begin_date, end_date, output_path) {
  # Load libraries
  library(quantmod)    
  library(dplyr)      
  library(zoo)       
  library(openxlsx)    
  
  # Initialize empty xts object to store prices
  prices <- xts()
  
  # Loop through each ticker in the provided list
  for (ticker in stock_namelist) {
    # Try to download adjusted close prices from Yahoo Finance
    stock_data <- try(
      getSymbols(ticker, src = 'yahoo',
                 from = begin_date,
                 to = end_date,
                 auto.assign = FALSE),
      silent = TRUE
    )
    
    # If data download is successful, extract Adjusted Close
    if (inherits(stock_data, "xts")) {
      adjusted_prices <- Ad(stock_data)
      colnames(adjusted_prices) <- ticker  # Rename column to the ticker
      prices <- cbind(prices, adjusted_prices)  # Merge with existing prices
    }
  }
  
  # Remove duplicate column names if any
  prices <- prices[, !duplicated(colnames(prices))]
  
  # Set time class as Date
  tclass(prices) <- "Date"
  
  # Filter out series that don't have complete data before start date
  valid_series <- apply(prices, 2, function(x) all(!is.na(x[index(x) < begin_date])))
  prices <- prices[, valid_series]
  
  # Forward-fill missing values to avoid look-ahead bias
  prices <- na.locf(prices, na.rm = FALSE)
  
  # Drop any remaining NA rows (e.g., incomplete start of series)
  prices <- na.omit(prices)
  
  # Handle date correctly
  df_to_save <- data.frame(Date = index(prices), coredata(prices))
  write.csv(df_to_save, file = output_path, row.names = FALSE)
  
  # Confirmation message
  message("✅ Dataset created at: ", output_path)
}
