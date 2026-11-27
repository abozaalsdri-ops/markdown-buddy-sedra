stoch_rsi <- function(data, period, k_period, d_period) {
  periods <- c(k_period, d_period)
  stopifnot(length(k_period) == 1L, length(d_period) == 1L,
            all(is.finite(periods)), all(periods >= 1),
            all(periods == floor(periods)))
  rsi_values <- rsi(data, period)
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  k_values <- rep(NA_real_, length(rsi_values))
  if (length(valid_rsi) > 0L) {
    limits <- range(valid_rsi)
    if (limits[2] > limits[1]) {
      k_values <- (rsi_values - limits[1]) / (limits[2] - limits[1])
    }
  }
  k_line <- if (length(k_values) >= k_period) sma(k_values, k_period) else numeric()
  d_line <- if (length(k_line) >= d_period) sma(k_line, d_period) else numeric()
  list(k_line = k_line, d_line = d_line)
}
