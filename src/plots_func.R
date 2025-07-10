plot_pair_log_price_change <- function(data_xts, ticker1, ticker2,
                                       start_date = NULL, end_date = NULL,
                                       colors = c("darkblue", "orange")) {
  if (!all(c(ticker1, ticker2) %in% colnames(data_xts))) {
    stop("One or both tickers not found in the dataset.")
  }
  
  # Subset by date if specified
  if (!is.null(start_date)) {
    data_xts <- data_xts[index(data_xts) >= as.Date(start_date)]
  }
  if (!is.null(end_date)) {
    data_xts <- data_xts[index(data_xts) <= as.Date(end_date)]
  }
  
  pair_xts <- data_xts[, c(ticker1, ticker2)]
  
  pair_df <- data.frame(Date = index(pair_xts), coredata(pair_xts)) %>%
    mutate(
      log_1 = log(.data[[ticker1]]),
      log_2 = log(.data[[ticker2]])
    ) %>%
    mutate(
      norm_1 = log_1 - log_1[1],
      norm_2 = log_2 - log_2[1]
    ) %>%
    select(Date, norm_1, norm_2) %>%
    rename(!!ticker1 := norm_1, !!ticker2 := norm_2) %>%
    pivot_longer(cols = c(ticker1, ticker2), names_to = "ETF", values_to = "LogReturn")
  
  ggplot(pair_df, aes(x = Date, y = LogReturn, color = ETF)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(
      title = paste("Log-Price Change of", ticker1, "and", ticker2),
      subtitle = paste("From", ifelse(is.null(start_date), min(index(data_xts)), start_date),
                       "to", ifelse(is.null(end_date), max(index(data_xts)), end_date)),
      x = "Date", y = "Log Price Change (from first obs)",
      color = "ETF"
    ) +
    scale_color_manual(values = colors)
}
