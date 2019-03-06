inverse_normal_transform <- function(x){
  int <- qnorm(
    (rank(x, na.last = "keep") + 0.5)/
      (length(x) + 1 - sum(is.na(x)))
  )
}