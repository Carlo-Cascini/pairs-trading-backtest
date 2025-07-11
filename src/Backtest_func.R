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

# Function to estimate initial beta using least squares on a specified training period
estimate_beta_LS <- function(Y_train) {
  lm_fit <- lm(Y_train[, 1] ~ Y_train[, 2]-1)
  return(list(beta = coef(lm_fit)[1]))  # Extract the slope as beta
}

# Function to estimate beta using Kalman filtering with customizable parameters
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
  init <- estimate_beta_LS(training_period)
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


# Function to fit the PCI model, store beta, and apply Kalman filter to get M_t for trading
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
generate_Z_score_EMA <- function(spread, n = 75) {
  spread.mean <- EMA(spread, n)
  spread.mean <- na.locf(spread.mean, fromLast = TRUE)
  spread.demeaned <- spread - spread.mean
  spread.var <- EMA(spread.demeaned^2, n)
  spread.var <- na.locf(spread.var, fromLast = TRUE)
  Z.score <- spread.demeaned / sqrt(spread.var)
  return(Z.score)
}




pairs_trading_PCI_tr <- function(Y, beta, M_t, name = NULL, threshold = 0.5, transaction_cost = 0.001, plot = FALSE) {
  # Compute spread using the state history from the PCI model
  w_spread <- cbind(1, -beta) / cbind(1 + beta, 1 + beta)
  
  # Compute Z-score based on the PCI model spread
  Z_score <- generate_Z_score_EMA(M_t)
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
    print(plot(exp(cumsum(portf_return)), main = paste("Cum P&L for spread based on", name)))  #NOTE  exp(cumsum(portf_return)) BECAUSE WE ARE DEALING WITH LOG RETURNS!
  }
  
  return(portf_return)
}


pairs_trading_kalm <- function(Y, beta, name = NULL, threshold = 0.5, transaction_cost = 0.001, plot = FALSE) {
  # Portfolio weights
  w_spread <- cbind(1, -beta) / cbind(1 + beta, 1 + beta)
  
  # Compute spread based on the dynamic beta
  spread <- compute_spread(Y, beta)
  
  # Generate Z-score
  Z_score <- generate_Z_score_EMA(spread)
  
  # Set thresholds
  threshold_long <- Z_score
  threshold_short <- Z_score
  threshold_short[] <- threshold
  threshold_long[] <- -threshold
  
  # Generate signals
  signal <- generate_signal(Z_score, threshold_long, threshold_short)
  
  # Compute lagged weights
  w_portf <- w_spread * lag.xts(cbind(signal, signal), k = 1)
  
  # Log returns
  X <- diff(log(Y))
  portf_return <- xts(rowSums(X * w_portf), index(X))
  
  # Transaction costs
  previous_signal <- lag.xts(signal, k = 1)
  new_trades <- (signal != 0) & (previous_signal == 0)
  closing_trades <- (signal == 0) & (previous_signal != 0)
  transaction_costs <- ifelse(new_trades | closing_trades, transaction_cost, 0)
  portf_return[new_trades | closing_trades] <- portf_return[new_trades | closing_trades] - transaction_costs[new_trades | closing_trades]
  
  # Clean NA
  portf_return[is.na(portf_return)] <- 0
  colnames(portf_return) <- name
  
  # Plot
  if (plot) {
    tmp <- cbind(Z_score, signal)
    colnames(tmp) <- c("Z-score", "signal")
    par(mfrow = c(2, 1))
    plot(tmp, legend.loc = "topleft", main = paste("Z-score and trading on spread based on", name))
    lines(threshold_short, lty = 2)
    lines(threshold_long, lty = 2)
    plot(exp(cumsum(portf_return)), main = paste("Cum P&L for spread based on", name))
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
      annualized_sd = NA
    ))
  }
  
  # Calculate cumulative return for log returns
  total_return <- exp(sum(portf_return)) - 1
  
  # Calculate average daily return and standard deviation
  avg_daily_return <- mean(portf_return, na.rm = TRUE)
  sd_return <- sd(portf_return, na.rm = TRUE)
  
  # Number of days in the return series
  num_days <- length(portf_return)
  
  # Annualized return calculation for log returns
  annualized_return <- exp(252 * avg_daily_return) - 1
  
  # Calculate annualized standard deviation
  annualized_sd <- sd_return * sqrt(252)
  
  # Convert annual risk-free rate to daily
  daily_risk_free_rate <- (1 + risk_free_rate)^(1/252) - 1
  
  # Sharpe ratio calculation
  sharpe_ratio <- if (sd_return != 0) {
    (avg_daily_return - daily_risk_free_rate) / sd_return * sqrt(252)
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






run_pairs_trading_strategy <- function(Y,
                                       tickers,
                                       test_start,
                                       test_end,
                                       training_years,
                                       transaction_cost,
                                       threshold,
                                       noise_matrix = 1e-3,
                                       smoothing_param = 50,
                                       risk_free_rate,
                                       plot) {
  # ---- 1. Calculate training period automatically ----
  test_start_date <- as.Date(test_start)
  test_end_date <- as.Date(test_end)
  train_end <- test_start_date - 1
  train_start <- seq(train_end, length = 2, by = paste0("-", training_years, " years"))[2]
  
  # ---- 2. Subset data ----
  Y_train <- Y[paste0(train_start, "::", train_end), tickers]
  Y_test  <- Y[paste0(test_start, "::", test_end), tickers]
  
  # ---- 3. PCI estimation and Kalman estimation ----
  result_PCI <- PCI_train_test(Y_train, Y_test)
  beta_est_PCI <- xts(rep(result_PCI$beta, nrow(Y_test)), order.by = index(Y_test))
  
  beta_result_kalman <- estimate_beta_Kalman(
    Y_test,
    training_period = Y_train,
    noise_matrix = noise_matrix,
    smoothing_param = smoothing_param
  )
  beta_est_kalman <- beta_result_kalman$beta
  
  # ---- 4. Run backtests ----
  ret_PCI <- pairs_trading_PCI_tr(
    Y_test, 
    beta_est_PCI, 
    M_t = result_PCI$M_t,
    name = paste("PCI:", paste(tickers, collapse = "-")),
    threshold = threshold,
    transaction_cost = transaction_cost,
    plot = plot
  )
  
  
  ret_KFB <- pairs_trading_kalm(
    Y = Y_test,
    beta = beta_est_kalman,
    name = paste("KFB:", paste(tickers, collapse = "-")),
    threshold = threshold,
    transaction_cost = transaction_cost,
    plot = plot  
  )
  
  
  # ---- 5. Compute performance ----
  perf_PCI <- calculate_performance(ret_PCI, risk_free_rate = risk_free_rate)
  perf_KFB <- calculate_performance(ret_KFB, risk_free_rate = risk_free_rate)
  
  # ---- 6. Summarize metrics ----
  perf_summary <- data.frame(
    Strategy = c("PCI", "KFB"),
    `Train Start` = as.character(train_start),
    `Train End` = as.character(train_end),
    `Test Start` = as.character(test_start),
    `Test End` = as.character(test_end),
    `Total Return (%)` = round(c(perf_PCI$total_return, perf_KFB$total_return) * 100, 2),
    `Annualized Return (%)` = round(c(perf_PCI$annualized_return, perf_KFB$annualized_return) * 100, 2),
    `Sharpe Ratio` = round(c(perf_PCI$sharpe_ratio, perf_KFB$sharpe_ratio), 2),
    `Annualized SD (%)` = round(c(perf_PCI$annualized_sd, perf_KFB$annualized_sd) * 100, 2),
    stringsAsFactors = FALSE
  )
  
  # ---- 7. Return output ----
  return(list(
    pair = tickers,
    training_window = c(train_start, train_end),
    test_window = c(test_start, test_end),
    PCI_return = ret_PCI,
    KFB_return = ret_KFB,
    beta_PCI = beta_est_PCI,
    beta_KFB = beta_est_kalman,
    performance = perf_summary
  ))
}
