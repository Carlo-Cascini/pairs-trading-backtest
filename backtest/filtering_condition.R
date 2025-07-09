# Define results folder path
results_folder <- "/Users/carlocascini/Desktop/pairs-trading/results/pairs"

# ---- Utility Functions ----

# Filtering function for PCI results
filter_by_conditions <- function(results) {
  if (!is.list(results) || length(results) == 0) {
    return(list())
  }
  Filter(function(x) {
    x$rho > 0.9 && x$rho < 0.98 &&
      x$rsq_MR > 0.8 &&
      !is.null(x$negloglik) && x$negloglik <= 0
  }, results)
}

# Create pairs list from filtered results
create_pairs_list <- function(filtered_results) {
  lapply(filtered_results, function(x) c(x$stock_a, x$stock_b))
}

# Process a single period and save filtered results
process_period <- function(year, half, results_folder) {
  period <- paste0(year, "_", half)
  res_file <- file.path(results_folder, paste0("res_", period, ".RData"))
  out_file <- file.path(results_folder, paste0("pairs_", period, ".RData"))
  
  # Check file exists
  if (!file.exists(res_file)) {
    cat("❌ File not found:", res_file, "\n")
    return(NULL)
  }
  
  # Load result into a temporary environment
  temp_env <- new.env()
  load(res_file, envir = temp_env)
  
  res_name <- paste0("res_", period)
  if (!exists(res_name, envir = temp_env)) {
    cat("❌ Object", res_name, "not found in", res_file, "\n")
    return(NULL)
  }
  
  results <- get(res_name, envir = temp_env)
  if (length(results) == 0) {
    cat("⚠️  Empty results in", res_name, "\n")
    return(NULL)
  }
  
  # Filter and extract pairs
  filtered_results <- filter_by_conditions(results)
  pairs <- create_pairs_list(filtered_results)
  
  # Assign to variables for saving
  assign(paste0("filtered_", period), filtered_results)
  assign(paste0("pairs_", period), pairs)
  
  # Save as RData
  save(list = c(paste0("filtered_", period), paste0("pairs_", period)),
       file = out_file)
  
  cat("✅ Saved filtered pairs for", period, "to", out_file, "\n")
  return(invisible(NULL))
}

# ---- Main Loop ----

for (year in 2014:2023) {
  for (half in c("H1", "H2")) {
    process_period(year, half, results_folder)
  }
}

# ---- Optional: Print all pairs ----

cat("\n📋 Complete List of Filtered Pairs:\n")
for (year in 2014:2023) {
  for (half in c("H1", "H2")) {
    pairs_file <- file.path(results_folder, paste0("pairs_", year, "_", half, ".RData"))
    if (file.exists(pairs_file)) {
      temp_env <- new.env()
      load(pairs_file, envir = temp_env)
      var_name <- paste0("pairs_", year, "_", half)
      if (exists(var_name, envir = temp_env)) {
        cat("\n▶️  Pairs List for", year, half, ":\n")
        print(get(var_name, envir = temp_env))
      }
    }
  }
}
