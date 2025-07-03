generate_periods <- function(start_date = as.Date("2010-01-01"), end_date = as.Date("2023-06-30")) {
  periods <- list()
  i <- 1
  current_start <- start_date
  
  while (TRUE) {
    train_start <- current_start
    train_end <- train_start + years(3) - days(1)
    if (train_end >= end_date) break
    
    test_start <- train_end + days(1)
    test_end <- test_start + months(6) - days(1)
    if (test_end > end_date) break
    
    year_tag <- year(test_start)
    half_tag <- ifelse(month(test_start) <= 6, "H1", "H2")
    file_name <- paste0("res_", year_tag, "_", half_tag, ".RData")
    
    periods[[i]] <- list(
      train_start = as.character(train_start),
      train_end = as.character(train_end),
      test_start = as.character(test_start),
      test_end = as.character(test_end),
      file = file_name
    )
    
    current_start <- current_start %m+% months(6)
    i <- i + 1
  }
  
  return(periods)
}


kalman_gain <- function(rho, sigmaM, sigmaR) {
  # Compute the denominator for the Kalman gain formula
  denom <- (sigmaR*(sqrt((rho + 1)^2 * sigmaR^2 + 4 * sigmaM^2) + rho * sigmaR + sigmaR) + 2 * sigmaM^2)
  
  # Compute Kalman gain components
  K1 <- 2 * sigmaM^2 / denom
  K2 <- 2* sigmaR / (sqrt((rho + 1)^2 * sigmaR^2 + 4 * sigmaM^2) - rho * sigmaR + sigmaR)
  
  # Return the Kalman gain as a vector
  return(c(K1, K2))
}

compute_spread <- function(Y, gamma, name = NULL) {
  # Ensure gamma is appropriately sized
  if (length(gamma) != nrow(Y)) {
    stop("Length of gamma must match the number of rows in Y")
  }
  
  gamma_matrix <- matrix(gamma, nrow = nrow(Y), ncol = 1)
  spread <- Y[, 1] - gamma_matrix * Y[, 2]
  spread_xts <- xts(spread, order.by = index(Y))
  colnames(spread_xts) <- name
  return(spread_xts)
}



kalman_estimate <- function(X, rho, sigmaM, sigmaR) {
  # Calculate Kalman gain
  K <- kalman_gain(rho, sigmaM, sigmaR)
  
  # Initialize vectors for estimates
  M <- numeric(length(X))
  R <- numeric(length(X))
  
  # Initial values
  M[1] <- 0
  R[1] <- X[1]
  
  # Iterate through observations
  for (i in 2:length(X)) {
    # Predicted value
    xhat <- rho * M[i - 1] + R[i - 1]
    
    # Prediction error
    e <- X[i] - xhat
    
    # Update estimates using Kalman gain
    M[i] <- rho * M[i - 1] + e * K[1]
    R[i] <- R[i - 1] + e * K[2]
  }
  
  # Return the estimated states
  return(list(M = M, R = R))
}


estimate_beta_LS <- function(Y_train) {
  lm_fit <- lm(Y_train[, 1] ~ Y_train[, 2]-1)
  return(list(beta = coef(lm_fit)[1]))  # Extract the slope as beta
}


estimate_beta_Kalman <- function(Y, training_period, noise_matrix = 1e-5, smoothing_param = 30) {
  T <- nrow(Y)
  
  # Initialize empty xts for storing beta estimates
  beta_Kalman_filtering <- xts(rep(NA, T), index(Y))
  colnames(beta_Kalman_filtering) <- "beta-Kalman"
  
  # Kalman filter parameters
  Tt <- diag(1)  # Only 1 state for beta
  Rt <- diag(1)
  Qt <- noise_matrix * diag(1)  # State transition variance based on input
  Zt <- array(as.vector(t(Y[, 2])), dim = c(1, 1, T))  # Time-varying design matrix
  Ht <- matrix(noise_matrix)  # Observation variance based on input
  
  # Estimate initial beta using least squares on the specified training period
  init <- estimate_beta_LS(Y_train)
  a1 <- matrix(init$beta, 1, 1)  # Initial beta state
  P1 <- 1e-4 * diag(1)  # Variance of initial point
  P1inf <- 0 * diag(1)
  
  # Create Kalman model
  model <- SSModel(as.matrix(Y[, 1]) ~ 0 + SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1, P1inf = P1inf), H = Ht)
  
  # Run Kalman filtering
  out <- KFS(model)
  beta_Kalman_filtering[] <- out$a[-1, 1]  # Extract beta estimates
  
  # Apply smoothing using rollapply from the zoo package
  beta_Kalman_filtering[] <- rollapply(as.numeric(beta_Kalman_filtering), width = smoothing_param, FUN = mean, fill = NA, align = "right")
  beta_Kalman_filtering <- na.locf(beta_Kalman_filtering, fromLast = TRUE)
  
  return(list(beta = beta_Kalman_filtering))
}


PCI_train_test <- function(Y_train, Y_test) {
  # Fit the model on the training data
  train <- fit.pci(Y_train)
  
  # Extract the trained beta parameter
  beta <- train$beta
  
  # Extract other parameters
  rho <- train$rho
  sigma_M <- train$sigma_M
  sigma_R <- train$sigma_R
  
  # Apply the model to the test data to calculate X_test
  X_test <- Y_test[, 1]  - beta * Y_test[, 2] 
  X_test <- as.numeric(X_test)
  
  # Perform Kalman filtering on the test data
  result_test <- kalman_estimate(X_test, rho, sigma_M, sigma_R)
  
  # Convert M_t (state vector) to an xts object using the time index of Y_test
  M_t_xts <- xts(result_test$M, order.by = index(Y_test))
  R_t_xts <- xts(result_test$R, order.by = index(Y_test))
  
  # Return the beta and M_t_xts (state vector for trading)
  return(list(
    beta = beta,
    M_t = M_t_xts,
    R_t = R_t_xts
  ))
}


generate_signal <- function(Z_score, threshold_long, threshold_short) {
  signal <- Z_score
  colnames(signal) <- "signal"
  signal[] <- NA
  
  # Initial position
  signal[1] <- 0
  if (Z_score[1] <= threshold_long[1]) {
    signal[1] <- 1
  } else if (Z_score[1] >= threshold_short[1]) {
    signal[1] <- -1
  }
  
  # Loop to generate signals
  for (t in 2:nrow(Z_score)) {
    if (signal[t-1] == 0) {
      if (Z_score[t] <= threshold_long[t]) {
        signal[t] <- 1
      } else if (Z_score[t] >= threshold_short[t]) {
        signal[t] <- -1
      } else {
        signal[t] <- 0
      }
    } else if (signal[t-1] == 1) {
      if (Z_score[t] >= 0) signal[t] <- 0
      else signal[t] <- signal[t-1]
    } else {
      if (Z_score[t] <= 0) signal[t] <- 0
      else signal[t] <- signal[t-1]
    }
  }
  return(signal)
}


generate_Z_score_EMA <- function(spread, n = 120) {
  spread.mean <- EMA(spread, n)
  spread.mean <- na.locf(spread.mean, fromLast = TRUE)
  spread.demeaned <- spread - spread.mean
  spread.var <- EMA(spread.demeaned^2, n)
  spread.var <- na.locf(spread.var, fromLast = TRUE)
  Z.score <- spread.demeaned / sqrt(spread.var)
  return(Z.score)
}



pairs_trading_PCI_tr <- function(Y, beta, name = NULL, threshold = 0.5, transaction_cost = 0.001, plot = FALSE) {
  # Compute spread using the state history from the PCI model
  w_spread <- cbind(1, -beta) / cbind(1 + beta, 1 + beta)
  
  # Compute Z-score based on the PCI model spread
  Z_score <- generate_Z_score_EMA(result_PCI$M_t)
  threshold_long <- Z_score
  threshold_short <- Z_score
  threshold_short[] <- threshold
  threshold_long[] <- -threshold
  
  # Generate trading signals
  signal <- generate_signal(Z_score, threshold_long, threshold_short)
  
  # Portfolio weights
  w_portf <- w_spread * lag.xts(cbind(signal, signal), k = 1)
  
  # Compute log-returns and portfolio returns
  X <- diff(log(Y))  # Compute log-returns from log-prices
  portf_return <- xts(rowSums(X * w_portf), index(X))
  
  # Identify the days where a new trade is initiated (signal changes from 0 to 1 or -1)
  previous_signal <- lag.xts(signal, k = 1)  # Previous day's signal
  new_trades <- (signal != 0) & (previous_signal == 0)
  closing_trades <- (signal == 0) & (previous_signal != 0)
  
  # Apply transaction costs only on the days a new trade is initiated or closed
  transaction_costs <- ifelse(new_trades | closing_trades, transaction_cost, 0)
  portf_return[new_trades | closing_trades] <- portf_return[new_trades | closing_trades] - transaction_costs[new_trades | closing_trades]
  
  # Replace NA values with 0 (initial day)
  portf_return[is.na(portf_return)] <- 0
  
  colnames(portf_return) <- name
  
  # plots
  if (plot) {
    tmp <- cbind(Z_score, signal)
    colnames(tmp) <- c("Z-score", "signal")
    par(mfrow = c(2, 1))
    { plot(tmp, legend.loc = "topleft",
           main = paste("Z-score and trading on spread based on", name))
      lines(threshold_short, lty = 2)
      print(lines(threshold_long, lty = 2)) }
    print(plot(cumprod(1 + portf_return), main = paste("Cum P&L for spread based on", name)))
  }
  
  return(portf_return)
}



pairs_trading_kalm <- function(Y, beta, name = NULL, threshold = 0.5, transaction_cost = 0.001, plot = FALSE) {
  # Compute spread using the state history from the PCI model
  w_spread <- cbind(1, -beta_est_kalman) / cbind(1 + beta_est_kalman, 1 + beta_est_kalman)
  spread<- compute_spread(Y_test,beta_est_kalman)
  # Compute Z-score based on the PCI model spread
  Z_score <- generate_Z_score_EMA(spread)
  threshold_long <- Z_score
  threshold_short <- Z_score
  threshold_short[] <- threshold
  threshold_long[] <- -threshold
  
  # Generate trading signals
  signal <- generate_signal(Z_score, threshold_long, threshold_short)
  
  # Portfolio weights
  w_portf <- w_spread * lag.xts(cbind(signal, signal), k = 1)
  
  # Compute log-returns and portfolio returns
  X <- diff(log(Y))  # Compute log-returns from log-prices
  portf_return <- xts(rowSums(X * w_portf), index(X))
  
  # Identify the days where a new trade is initiated (signal changes from 0 to 1 or -1)
  previous_signal <- lag.xts(signal, k = 1)  # Previous day's signal
  new_trades <- (signal != 0) & (previous_signal == 0)
  closing_trades <- (signal == 0) & (previous_signal != 0)
  
  # Apply transaction costs only on the days a new trade is initiated or closed
  transaction_costs <- ifelse(new_trades | closing_trades, transaction_cost, 0)
  portf_return[new_trades | closing_trades] <- portf_return[new_trades | closing_trades] - transaction_costs[new_trades | closing_trades]
  
  # Replace NA values with 0 (initial day)
  portf_return[is.na(portf_return)] <- 0
  
  colnames(portf_return) <- name
  
  # plots
  if (plot) {
    tmp <- cbind(Z_score, signal)
    colnames(tmp) <- c("Z-score", "signal")
    par(mfrow = c(2, 1))
    { plot(tmp, legend.loc = "topleft",
           main = paste("Z-score and trading on spread based on", name))
      lines(threshold_short, lty = 2)
      print(lines(threshold_long, lty = 2)) }
    print(plot(cumprod(1 + portf_return), main = paste("Cum P&L for spread based on", name)))
  }
  
  return(portf_return)
}


calculate_performance <- function(portf_return, risk_free_rate = 0) {
  # Ensure portf_return is numeric and remove NA values
  portf_return <- na.omit(as.numeric(portf_return))
  
  # Check if portf_return is empty
  if (length(portf_return) == 0) {
    return(list(
      total_return = NA,
      annualized_return = NA,
      sharpe_ratio = NA,
      num_days = NA,
      sd_return = NA
    ))
  }
  
  # Calculate cumulative return
  cum_return <- cumprod(1 + portf_return) 
  total_return <- cum_return[length(cum_return)] - 1
  
  # Calculate average daily return and standard deviation
  avg_daily_return <- mean(portf_return, na.rm = TRUE)
  sd_return <- sd(portf_return, na.rm = TRUE)
  
  # Number of days in the return series
  num_days <- length(portf_return)
  
  
  # Annualized return calculation
  annualized_return <- (1 + avg_daily_return) ^ (252) - 1
  annualized_sd<-sd_return*sqrt(252)
  ann_risk_free<-risk_free_rate^1/252
  # Sharpe ratio calculation
  sharpe_ratio <- if (sd_return != 0) {
    (avg_daily_return - ann_risk_free) / sd_return * sqrt(252)
  } else {
    NA
  }
  
  # Return performance metrics as a list
  return(list(
    total_return = total_return,
    annualized_return = annualized_return,
    sharpe_ratio = sharpe_ratio,
    num_days = num_days,
    annualized_sd = annualized_sd
  ))
}



combine_daily_returns <- function(results_list) {
  # Merge all results into a single xts object
  combined_returns <- do.call(merge, results_list)
  
  # Calculate the daily average return across all pairs
  avg_daily_returns <- rowMeans(combined_returns, na.rm = TRUE)
  
  return(xts(avg_daily_returns, order.by = index(combined_returns)))
}

format_percentage <- function(x) {
  sprintf("%.2f%%", as.numeric(x))
}


extract_metrics <- function(perf, strategy_name, start_date, end_date) {
  c(
    Strategy = strategy_name,
    "Start Date" = start_date,
    "End Date" = end_date,
    "Total Return (%)" = format_percentage(perf$total_return * 100),
    "Annualized Return (%)" = format_percentage(perf$annualized_return * 100),
    "Sharpe Ratio (%)" = format_percentage(perf$sharpe_ratio),
    "Annualized Standard Deviation (%)" = format_percentage(perf$annualized_sd * 100)
  )
}


process_window <- function(train_start, train_end, test_start, test_end, pairs_list, Y) {
  train_m <- paste0(train_start, "::", train_end)
  test_m <- paste0(test_start, "::", test_end)
  
  results_PCI <- list()
  results_kalm <- list()
  
  for (pair in pairs_list) {
    pair_name <- paste(pair, collapse = "-")
    Y_train <- Y[train_m, pair]
    Y_test  <- Y[test_m, pair]
    
    result_PCI <- PCI_train_test(Y_train, Y_test)
    beta_kalman <- estimate_beta_Kalman(Y_test, training_period = Y_train, noise_matrix = 1e-3, smoothing_param = 30)$beta
    beta_PCI_xts <- xts(rep(result_PCI$beta, nrow(Y_test)), order.by = index(Y_test))
    
    ret_PCI <- pairs_trading_PCI_tr(Y_test, beta_PCI_xts, plot = FALSE, transaction_cost = 0.001, threshold = 1)
    ret_kalm <- pairs_trading_kalm(Y_test, beta_kalman, plot = FALSE, transaction_cost = 0.001, threshold = 1)
    
    results_PCI[[pair_name]] <- ret_PCI
    results_kalm[[pair_name]] <- ret_kalm
  }
  
  # Combine and compute equity curves
  returns_PCI <- combine_daily_returns(results_PCI)
  returns_kalm <- combine_daily_returns(results_kalm)
  equity_curve_PCI <- cumprod(1 + returns_PCI) - 1
  equity_curve_kalm <- cumprod(1 + returns_kalm) - 1
  
  # Performance
  perf_PCI <- calculate_performance(returns_PCI, risk_free_rate = 0.02)
  perf_KFB <- calculate_performance(returns_kalm, risk_free_rate = 0.02)
  
  metrics_PCI <- c(perf_PCI$total_return * 100, perf_PCI$annualized_return * 100, perf_PCI$sharpe_ratio, perf_PCI$annualized_sd * 100)
  metrics_KFB <- c(perf_KFB$total_return * 100, perf_KFB$annualized_return * 100, perf_KFB$sharpe_ratio, perf_KFB$annualized_sd * 100)
  
  res <- rbind(
    c("PCI", test_start, test_end, metrics_PCI),
    c("KFB", test_start, test_end, metrics_KFB)
  )
  
  colnames(res) <- c("Strategy", "Start Date", "End Date", "Total Return (%)", "Annualized Return (%)", "Sharpe Ratio", "Annualized Volatility (%)")
  df <- as.data.frame(res, stringsAsFactors = FALSE)
  df[, 4:7] <- lapply(df[, 4:7], format_percentage)
  
  return(list(performance = df,
              equity = data.frame(Date = index(equity_curve_PCI), PCI = coredata(equity_curve_PCI), Kalman = coredata(equity_curve_kalm))))
}


