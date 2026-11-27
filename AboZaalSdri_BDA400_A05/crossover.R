crossover <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  signals <- rep("None", length(arr1))
  previous <- seq_len(max(0L, length(arr1) - 1L))
  current <- previous + 1L
  up <- arr1[current] > arr2[current] & arr1[previous] <= arr2[previous]
  down <- arr1[current] < arr2[current] & arr1[previous] >= arr2[previous]
  signals[current[which(up)]] <- "Up"
  signals[current[which(down)]] <- "Down"
  signals
}
