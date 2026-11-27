rsi <- function(data, period) {
  stopifnot(is.numeric(data), all(is.finite(data)), length(period) == 1L,
            is.finite(period), period >= 1, period == floor(period))
  rsi_values <- rep(NA_real_, length(data))
  if (length(data) <= period) {
    return(rsi_values)
  }

  changes <- diff(data)
  gains <- pmax(changes, 0)
  losses <- pmax(-changes, 0)
  avg_gain <- mean(gains[seq_len(period)])
  avg_loss <- mean(losses[seq_len(period)])

  # The initial averages already include the first full window.
  for (i in seq.int(period + 1L, length(data))) {
    if (i > period + 1L) {
      avg_gain <- (avg_gain * (period - 1L) + gains[i - 1L]) / period
      avg_loss <- (avg_loss * (period - 1L) + losses[i - 1L]) / period
    }
    rsi_values[i] <- if (avg_gain + avg_loss == 0) {
      50
    } else {
      100 * avg_gain / (avg_gain + avg_loss)
    }
  }
  rsi_values
}
