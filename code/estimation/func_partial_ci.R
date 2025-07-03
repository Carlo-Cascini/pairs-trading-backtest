# --- Functions for Partial CI Analysis ---

calculate_rsq_MR <- function(stock_a, stock_b, start_date, end_date) {
  tryCatch({
    a_prices <- data_xts[index(data_xts) >= start_date & index(data_xts) <= end_date, stock_a, drop = FALSE]
    b_prices <- data_xts[index(data_xts) >= start_date & index(data_xts) <= end_date, stock_b, drop = FALSE]
    
    prices <- merge(a_prices, b_prices, all = FALSE)
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
    message(paste("Error with", stock_a, "and", stock_b, ":", e$message))
    return(NULL)
  })
}

process_window <- function(start_date, end_date, file_name) {
  start_time <- Sys.time()
  
  analysis_start_date <- as.Date(start_date)
  analysis_end_date <- as.Date(end_date)
  
  message(paste("Processing period:", analysis_start_date, "to", analysis_end_date))
  
  results <- lapply(stock_pairs, function(pair) {
    calculate_rsq_MR(pair[1], pair[2], analysis_start_date, analysis_end_date)
  })
  
  results <- Filter(function(x) !is.null(x), results)
  
  var_name <- gsub("\\.RData$", "", basename(file_name))
  assign(var_name, results, envir = .GlobalEnv)
  
  save(list = var_name, file = file_name)
  
  elapsed_time <- Sys.time() - start_time
  message(paste("Number of results:", length(results)))
  message(paste("Time taken for period:", analysis_start_date, "to", analysis_end_date, ":", format(elapsed_time, digits = 2)))
  
  return(file_name)
}

