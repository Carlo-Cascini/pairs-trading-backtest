# --- Partial Cointegration Estimation Functions ---

calculate_rsq_MR <- function(stock_a, stock_b, start_date, end_date, data_xts) {
  tryCatch({
    a_prices <- data_xts[paste0(start_date, "/", end_date), stock_a, drop = FALSE]
    b_prices <- data_xts[paste0(start_date, "/", end_date), stock_b, drop = FALSE]
    
    prices <- merge(a_prices, b_prices, all = FALSE)
    
    if (nrow(prices) < 20) stop("Too few observations")
    
    fit <- fit.pci(prices[, 1], prices[, 2])
    
    return(list(
      stock_a = stock_a,
      stock_b = stock_b,
      rsq_MR = fit$pvmr,
      rho = fit$rho,
      negloglik = fit$negloglik,
      beta = fit$beta,
      sigmak = fit$sigma_M,
      sigmaz = fit$sigma_R
    ))
  }, error = function(e) {
    message(paste("⚠️ Error with", stock_a, "&", stock_b, ":", e$message))
    return(NULL)
  })
}

process_window <- function(stock_pairs, data_xts, start_date, end_date, file_path) {
  analysis_start <- as.Date(start_date)
  analysis_end <- as.Date(end_date)
  
  message(paste("🔄 Processing period:", analysis_start, "to", analysis_end))
  
  results <- lapply(stock_pairs, function(pair) {
    calculate_rsq_MR(pair[1], pair[2], analysis_start, analysis_end, data_xts)
  })
  
  results <- Filter(Negate(is.null), results)
  
  var_name <- gsub("\\.RData$", "", basename(file_path))
  assign(var_name, results, envir = .GlobalEnv)
  save(list = var_name, file = file_path)
  
  message(paste("✅ Saved", length(results), "results in", file_path))
  return(file_path)
}

generate_rolling_windows <- function(start_date, end_date, window_years = 3, step_months = 6) {
  windows <- list()
  i <- 1
  current_start <- start_date
  
  while (TRUE) {
    current_end <- current_start + years(window_years) - days(1)
    if (current_end > end_date) break
    
    year_tag <- year(current_end)
    half_tag <- ifelse(month(current_end) <= 6, "H1", "H2")
    file_name <- paste0("res_", year_tag, "_", half_tag, ".RData")
    
    windows[[i]] <- list(
      start = as.character(current_start),
      end = as.character(current_end),
      file = file_name
    )
    
    current_start <- current_start %m+% months(step_months)
    i <- i + 1
  }
  
  return(windows)
}

run_partial_ci_backtest <- function(stock_pairs, data_xts, estimation_years, rolling_step_months, save_dir) {
  dir.create(save_dir, showWarnings = FALSE)
  
  full_start <- start(data_xts)
  full_end <- end(data_xts)
  rolling_windows <- generate_rolling_windows(full_start, full_end, estimation_years, rolling_step_months)
  
  lapply(rolling_windows, function(win) {
    file_path <- file.path(save_dir, win$file)
    process_window(stock_pairs, data_xts, win$start, win$end, file_path)
  })
}
