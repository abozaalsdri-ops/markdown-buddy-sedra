stdev <- function(data) {
  stopifnot(is.numeric(data), length(data) > 0L)
  sqrt(mean((data - mean(data))^2))
}
