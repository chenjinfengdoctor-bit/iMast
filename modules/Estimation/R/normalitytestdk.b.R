# This file is a generated template, your changes will not be overwritten
dk.test <- function(x) {
  n <- length(x)
  mx <- mean(x)
  m4 <- sum((x - mx)^4) / n
  m2 <- sum((x - mx)^2) / n
  b2 <- m4 / m2^2
  G <- (b2 - (3 * n - 3) / (n + 1)) /
    sqrt(
      24 * n * (n - 2) * (n - 3) /
        (n + 1)^2 / (n + 3) / (n + 5)
    )
  E <- 6 * (n^2 - 5 * n + 2) / (n + 7) / (n + 9) *
    sqrt(6 * (n + 3) * (n + 5) / n / (n - 2) / (n - 3))
  A <- 6 + 8 / E * (2 / E + sqrt(1 + 4 / E^2))
  zk <- ((1 - 2 / 9 / A) - (
    (1 - 2 / A) /
      (1 + G * sqrt(2 / (A - 4)))
  )^(1 / 3)) /
    sqrt(2 / 9 / A)
  return(
    list(
      statistic = zk,
      p.value = 2 * (1 - pnorm(abs(zk))),
      m4 = m4,
      m2 = m2,
      b2 = b2
    )
  )
}
NormalityTestDKClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "NormalityTestDKClass",
    inherit = NormalityTestDKBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        dk <- dk.test(self$data[, self$options$x])
        self$results$result$setContent(
          c(
            sprintf("statistic = %f", dk$statistic),
            sprintf("p         = %f", dk$p.value)
            # sprintf("$m_2$       = %f", dk$m2),
            # sprintf("$m_4$       = %f", dk$m4),
            # sprintf("$b_2$        = %f", dk$b2)
          )
        )
      }
    )
  )
}
