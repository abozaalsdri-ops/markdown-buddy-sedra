macd <- function(data, short_period, long_period, signal_period) {
  macd_line <- ema(data, short_period) - ema(data, long_period)
  signal_line <- ema(macd_line, signal_period)
  list(macd_line = macd_line, signal_line = signal_line,
       histogram = macd_line - signal_line)
}
