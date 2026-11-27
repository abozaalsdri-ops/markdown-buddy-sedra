crossunder <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  signals <- rep("False", length(arr1))
  if (length(signals) > 0L) {
    signals[1] <- "None"
  }
  previous <- seq_len(max(0L, length(arr1) - 1L))
  current <- previous + 1L
  down <- arr1[current] < arr2[current] & arr1[previous] >= arr2[previous]
  signals[current[which(down)]] <- "True"
  signals
}
