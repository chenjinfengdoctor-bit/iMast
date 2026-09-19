# This file is a generated template, your changes will not be overwritten
mi.cv <- function(n) {
  m <- as.integer(8e5 / n)
  cv <- numeric(m)
  for (i in 1:m) {
    data <- rnorm(n)
    stat <- mi.statistic(data)
    cv[i] <- stat
  }
  cv
}
mi.statistic <- function(x) {
  M <- median(x)
  n <- length(x)
  aux1 <- x - M
  xtmp <- abs(aux1)
  A <- 9.0 * median(xtmp)
  z <- aux1 / A
  term1 <- 0
  term2 <- 0
  term3 <- sum(aux1^2)
  for (i in 1:n) {
    if (abs(z[i]) < 1) {
      z2 <- z[i]^2
      term1 <- term1 + aux1[i]^2 * (1 - z2)^4
      term2 <- term2 + (1 - z2) * (1 - 5 * z2)
    }
  }
  Sb2 <- n * term1 / term2^2
  statIn <- (term3 / (n - 1)) / Sb2
  statIn
}
mi.test <- function(x) {
  s <- mi.statistic(x)
  cv <- mi.cv(length(x))
  return(list(
    statistic = s,
    p.value = 1 - ecdf(cv)(s)
  ))
}
NormalityTestMIClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "NormalityTestMIClass",
    inherit = NormalityTestMIBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        mi <- mi.test(self$data[, self$options$x])
        self$results$result$setContent(
          c(
            sprintf("statistic = %f", mi$statistic),
            sprintf("p         = %f", mi$p.value)
          )
        )
      }
    )
  )
}
