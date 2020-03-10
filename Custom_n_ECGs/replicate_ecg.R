# Replicate an ecg wave n times
rep_ecg <- function(ecg, n) {
  lenx <- nrow(ecg)
  n_waves <- do.call("rbind", replicate(n, ecg, simplify = FALSE))
  for (i in seq(0, n-1)) {
    beg <- i*lenx
    end <- (i+1)*lenx - 1
    n_waves$x[beg:end] <- n_waves$x[beg:end] + i
  }
  return(n_waves)
}