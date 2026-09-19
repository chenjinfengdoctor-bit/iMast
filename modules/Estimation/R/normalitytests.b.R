# This file is a generated template, your changes will not be overwritten
s.statistic <- function(x) {
  xm <- mean(x)
  n <- length(x)
  d <- x - xm
  m2 <- sum(d^2 / n)
  m3 <- sum(d^3 / n)
  s <- abs(m3) / sqrt(m2^3)
  return(s)
}
s.cv <- function(n) {
  m <- as.integer(8e6/n)
  cv <- numeric(m)
  for (i in 1:m) {
    data <- rnorm(n)
    stat <- s.statistic(data)
    cv[i] <- stat
  }
  cv
}
s.test <- function(x) {
  s <- s.statistic(x)
  cv <- s.cv(length(x))
  return(list(
    statistics = s,
    p.value = 1 - ecdf(cv)(s)
  ))
}
NormalityTestSClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "NormalityTestSClass",
    inherit = NormalityTestSBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        s <- s.test(self$data[, self$options$x])
        self$results$result$setContent(
          c(
            sprintf("statistic = %f", s$statistic),
            sprintf("p         = %f", s$p.value)
          )
        )
      }
    )
  )
}
