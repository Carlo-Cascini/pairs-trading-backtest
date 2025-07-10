# ---- Utility Functions ----

# Filtering function with threshold parameters
filter_by_conditions <- function(results, rho_min = 0.9, rho_max = 0.98, rsq_min = 0.8, loglik_max = 0) {
  if (!is.list(results) || length(results) == 0) return(list())
  
  Filter(function(x) {
    !is.null(x$rho) && x$rho > rho_min && x$rho < rho_max &&
      !is.null(x$rsq_MR) && x$rsq_MR > rsq_min &&
      !is.null(x$negloglik) && x$negloglik <= loglik_max
  }, results)
}

# Create pairs list from filtered results
create_pairs_list <- function(filtered_results) {
  lapply(filtered_results, function(x) c(x$stock_a, x$stock_b))
}

# Process a single period and save filtered results
process_period <- function(year, half, results_folder, rho_min = 0.9, rho_max = 0.98,
                           rsq_min = 0.8, loglik_max = 0, save_dir = results_folder) {
  period <- paste0(year, "_", half)
  res_file <- file.path(results_folder, paste0("res_", period, ".RData"))
  out_file <- file.path(save_dir, paste0("pairs_", period, ".RData"))
  
  if (!file.exists(res_file)) {
    cat(" File not found:", res_file, "\n")
    return(NULL)
  }
  
  temp_env <- new.env()
  load(res_file, envir = temp_env)
  
  res_name <- paste0("res_", period)
  if (!exists(res_name, envir = temp_env)) {
    cat(" Object", res_name, "not found in", res_file, "\n")
    return(NULL)
  }
  
  results <- get(res_name, envir = temp_env)
  if (length(results) == 0) {
    cat("️  Empty results in", res_name, "\n")
    return(NULL)
  }
  
  # Filter and extract pairs
  filtered_results <- filter_by_conditions(results, rho_min, rho_max, rsq_min, loglik_max)
  pairs <- create_pairs_list(filtered_results)
  
  # Save filtered results
  assign(paste0("filtered_", period), filtered_results)
  assign(paste0("pairs_", period), pairs)
  
  save(list = c(paste0("filtered_", period), paste0("pairs_", period)), file = out_file)
  cat(" Saved filtered pairs for", period, "to", out_file, "\n")
  
  invisible(NULL)
}
