ema <- function(data, period) {
  stopifnot(is.numeric(data), length(period) == 1L,
            is.finite(period), period >= 1, period == floor(period))
  ema_values <- numeric(length(data))
  multiplier <- 2 / (period + 1)
  for (i in seq_along(data)) {
    ema_values[i] <- if (i == 1L) {
      data[i]
    } else {
      previous <- ema_values[i - 1L]
      previous + (data[i] - previous) * multiplier
    }
  }
  ema_values
}
