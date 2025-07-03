# Load libraries
library(readxl)
library(xts)
library(quantmod)
library(zoo)
library(TTR)
library(KFAS)
library(partialCI)
library(ggplot2)
library(writexl)
library(tidyr)
library(dplyr)

# Load custom functions
source("/Users/carlocascini/Desktop/pairs-trading/code/backtest/Backtest_functions.R")

# Read ETF data
data <- read.csv("/Users/carlocascini/Desktop/pairs-trading/code/data/67_cleaned_etfs.csv")
colnames(data)[1] <- "Date"
data$Date <- as.Date(data$Date)
Y <- xts(data[, -1], order.by = data$Date)

# Define period
period <- "2017_H1"

# Load filtered pairs
pairs_path <- file.path("/Users/carlocascini/Desktop/pairs-trading/results/pairs", paste0("pairs_", period, ".RData"))
temp_env <- new.env()
load(pairs_path, envir = temp_env)

pairs_var_name <- paste0("pairs_", period)
if (!exists(pairs_var_name, envir = temp_env)) {
  stop("Pairs list not found in ", pairs_path)
}
pairs_list <- get(pairs_var_name, envir = temp_env)

# --- Define training and testing windows ---
get_train_test_dates <- function(period) {
  parts <- strsplit(period, "_")[[1]]
  year <- as.numeric(parts[1])
  half <- parts[2]
  
  if (half == "H1") {
    test_start <- as.Date(paste0(year, "-01-01"))
    test_end   <- as.Date(paste0(year, "-06-30"))
  } else {
    test_start <- as.Date(paste0(year, "-07-01"))
    test_end   <- as.Date(paste0(year, "-12-31"))
  }
  
  train_start <- test_start %m-% years(3)
  train_end   <- test_start %m-% days(1)
  
  return(list(
    train_m = paste0(train_start, "::", train_end),
    test_m = paste0(test_start, "::", test_end),
    test_start = test_start,
    test_end = test_end
  ))
}

# Assign rolling windows
dates <- get_train_test_dates(period)
train_m <- dates$train_m
test_m <- dates$test_m
test_start <- dates$test_start
test_end <- dates$test_end

# Run backtest
results_PCI <- list()
results_kalm <- list()

for (pair in pairs_list) {
  pair_name <- paste(pair, collapse = "-")
  
  Y_train <- Y[train_m, pair]
  Y_test  <- Y[test_m, pair]
  
  result_PCI <- PCI_train_test(Y_train, Y_test)
  beta_result_kalman <- estimate_beta_Kalman(Y_test, training_period = Y_train, noise_matrix = 1e-3, smoothing_param = 30)
  
  beta_est_PCI <- result_PCI$beta
  beta_est_kalman <- beta_result_kalman$beta
  
  beta_est_PCI_xts <- xts(rep(beta_est_PCI, nrow(Y_test)), order.by = index(Y_test))
  
  portf_return_PCI <- pairs_trading_PCI_tr(Y_test, beta_est_PCI_xts, plot = FALSE, transaction_cost = 0.001, threshold = 1, name = paste("PCI MODEL ON", pair_name))
  portf_return_kalm <- pairs_trading_kalm(Y_test, beta_est_kalman, plot = FALSE, transaction_cost = 0.001, threshold = 1, name = paste("KALMAN MODEL ON", pair_name))
  
  results_PCI[[pair_name]] <- portf_return_PCI
  results_kalm[[pair_name]] <- portf_return_kalm
}

# Combine returns
combined_PCI_returns <- combine_daily_returns(results_PCI)
combined_kalm_returns <- combine_daily_returns(results_kalm)

# Equity curves
equity_curve_PCI <- cumprod(1 + combined_PCI_returns) - 1
equity_curve_kalm <- cumprod(1 + combined_kalm_returns) - 1

# Plot equity curves
equity_data <- data.frame(
  Date = index(equity_curve_PCI),
  PCI = coredata(equity_curve_PCI),
  Kalman = coredata(equity_curve_kalm)
)

equity_long <- pivot_longer(equity_data, cols = c("PCI", "Kalman"), names_to = "Strategy", values_to = "Cumulative_Return")

ggplot(equity_long, aes(x = Date, y = 1 + Cumulative_Return, color = Strategy)) +
  geom_line() +
  labs(
    title = paste("Equity Curves for PCI and Kalman -", period),
    x = "Date", y = "Cumulative Return", color = "Strategy"
  ) +
  scale_color_manual(values = c("PCI" = "blue", "Kalman" = "red")) +
  theme_minimal() +
  theme(
    legend.position = c(0.1, 0.9),
    legend.justification = c("left", "top"),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12, face = "bold")
  )

# Performance metrics
perf_PCI <- calculate_performance(combined_PCI_returns, risk_free_rate = 0.02)
perf_KFB <- calculate_performance(combined_kalm_returns, risk_free_rate = 0.02)

# Assemble performance summary
perf_res <- rbind(
  c("PCI", as.character(test_start), as.character(test_end),
    format_percentage(perf_PCI$total_return * 100),
    format_percentage(perf_PCI$annualized_return * 100),
    format_percentage(perf_PCI$sharpe_ratio),
    format_percentage(perf_PCI$annualized_sd * 100)),
  
  c("KFB", as.character(test_start), as.character(test_end),
    format_percentage(perf_KFB$total_return * 100),
    format_percentage(perf_KFB$annualized_return * 100),
    format_percentage(perf_KFB$sharpe_ratio),
    format_percentage(perf_KFB$annualized_sd * 100))
)

colnames(perf_res) <- c("Strategy", "Start Date", "End Date",
                        "Total Return (%)", "Annualized Return (%)",
                        "Sharpe Ratio", "Annualized Volatility (%)")
rownames(perf_res) <- c("PCI_Strategy", "Kalman_Strategy")

perf_df <- as.data.frame(perf_res, stringsAsFactors = FALSE)
print(perf_df)

