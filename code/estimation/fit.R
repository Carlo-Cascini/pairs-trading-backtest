# Load necessary libraries
library(devtools)
library(quantmod)
library(xts)
library(zoo)
library(partialAR)
library(partialCI)
library(TTR)
library(readxl)
library(dplyr)
library(parallel)
library(foreach)
library(doParallel)
library(pbapply)
library(lubridate)

# Read data
data <- read_excel("/Users/carlocascini/Desktop/pairs-trading/code/data/67_cleaned_etfs.xlsx")
data$...1 <- as.Date(data$...1)
data_xts <- xts(data[, -1], order.by = data$...1)

# Load function definitions
source("/Users/carlocascini/Desktop/pairs-trading/code/estimation/func_partial_ci.R")

# Define tickers and pairs
stock_tickers <- c("EWA", "EWK", "EWO", "EWC", "EWQ", "EWG", "EWH")
stock_pairs <- combn(stock_tickers, 2, simplify = FALSE)

# Define save directory
save_dir <- "/Users/carlocascini/Desktop/pairs-trading/results/"


# Define start and end of the total sample
full_start <- as.Date("2010-01-01")
full_end <- as.Date("2023-06-30")

# Create a list to store the windows
rolling_windows <- list()

# Initialize index
i <- 1
current_start <- full_start

while (TRUE) {
  current_end <- current_start + years(3) - days(1)  # 3-year window
  if (current_end > full_end) break  # Stop when exceeding last available date
  
  # File name pattern e.g., res_2014_H1, res_2014_H2, etc.
  year_tag <- year(current_end)
  half_tag <- ifelse(month(current_end) <= 6, "H1", "H2")
  file_name <- paste0("res_", year_tag, "_", half_tag, ".RData")
  
  rolling_windows[[i]] <- list(
    start = as.character(current_start),
    end = as.character(current_end),
    file = file_name
  )
  
  # Move forward by 6 months
  current_start <- current_start %m+% months(6)
  i <- i + 1
}




results <- pblapply(rolling_windows, function(win) {
  file_path <- paste0(save_dir, win$file)
  process_window(win$start, win$end, file_path)
})