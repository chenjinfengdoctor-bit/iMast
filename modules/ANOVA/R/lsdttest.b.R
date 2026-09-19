# This file is a generated template, your changes will not be overwritten

LSDTTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "LSDTTestClass",
    inherit = LSDTTestBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        mi <- self$options$mi
        mj <- self$options$mj
        ni <- self$options$ni
        nj <- self$options$nj
        ve <- self$options$ve
        mse <- self$options$mse
        s <- sqrt(mse * (1 / ni + 1 / nj))
        lsdt <- (mi - mj) / s
        a <- as.numeric(self$options$a)
        p <- 1 - pt(abs(lsdt), ve)
        if (p <= a) {
          self$results$conclusion$setTitle(
            "$H_0(\\mu_i=\\mu_j)$: Reject"
          )
        } else {
          self$results$conclusion$setTitle(
            "$H_0(\\mu_i=\\mu_j)$: Not Reject"
          )
        }
        self$results$conclusion$setContent(
          sprintf("$LSD-t=%.2f \\\\ P=%f$", lsdt, p)
        )
      }
    )
  )
}
