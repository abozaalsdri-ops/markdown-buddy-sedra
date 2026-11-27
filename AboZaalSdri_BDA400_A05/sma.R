sma <- function(data, period) {
  stopifnot(is.numeric(data), length(period) == 1L,
            is.finite(period), period >= 1, period == floor(period))
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  vapply(seq_len(length(data) - period + 1L), function(i) {
    mean(data[seq.int(i, length.out = period)])
  }, numeric(1))
}
