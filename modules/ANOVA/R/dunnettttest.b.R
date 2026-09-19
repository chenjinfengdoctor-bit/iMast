# This file is a generated template, your changes will not be overwritten
DunnettTTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "DunnettTTestClass",
    inherit = DunnettTTestBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        mi <- self$options$mi
        m0 <- self$options$m0
        ni <- self$options$ni
        n0 <- self$options$n0
        ve <- self$options$ve
        mse <- self$options$mse
        T <- self$options$t
        s <- sqrt(mse * (1 / ni + 1 / n0))
        t <- (mi - m0) / s
        a <- as.numeric(self$options$a)
        p <- 2 * (1 - ptukey(abs(t) * sqrt(2), T, ve))
        dtt <- qtukey(1 - a, T, ve) / sqrt(2)
        if (p <= a) {
          self$results$conclusion$setTitle(
            "$H_0(\\mu_i=\\mu_0)$: Reject"
          )
        } else {
          self$results$conclusion$setTitle(
            "$H_0(\\mu_i=\\mu_0)$: Not Reject"
          )
        }
        self$results$conclusion$setContent(
          sprintf("Dunnett-$t=%.2f \\\\ P=%f$", t, p)
        )
      }
    )
  )
}
