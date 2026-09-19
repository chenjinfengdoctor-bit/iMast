# This file is a generated template, your changes will not be overwritten

BartlettTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "BartlettTestClass",
    inherit = BartlettTestBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        v <- self$data[, self$options$v]
        n <- self$data[, self$options$n]
        sc2 <- sum((n - 1) * v) / sum(n - 1)
        g <- length(v)
        chi2 <- sum((n - 1) * log(sc2 / v)) /
          (1 + (sum((n - 1)^-1) - sum(n - 1)^-1) / (3 * (g - 1)))
        p <- 1 - pchisq(chi2, g - 1)
        a <- as.numeric(self$options$a)
        pa <- qchisq(1 - a, g - 1)
        if (p <= a) {
          self$results$conclusion$setTitle(
            "$H_0(\\sigma_1^2=\\cdots=\\sigma_g^2=\\sigma^2)$: Reject"
          )
        } else {
          self$results$conclusion$setTitle(
            "$H_0(\\sigma_1^2=\\cdots=\\sigma_g^2=\\sigma^2)$: Not Reject"
          )
        }
        self$results$conclusion$setContent(
          sprintf(
            "$\\chi^2=%.4f \\\\ P=%.4f \\\\ P_\\{%.3f,%d\\}=%.4f$",
            chi2, p, a, g - 1, pa
          )
        )
      }
    )
  )
}
