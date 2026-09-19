# This file is a generated template, your changes will not be overwritten

OneSampleTTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "OneSampleTTestClass",
    inherit = OneSampleTTestBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        sm <- self$options$sm
        pm <- self$options$pm
        ssd <- self$options$ssd
        sn <- self$options$sn
        a <- as.numeric(self$options$a)
        t <- (sm - pm) / ssd * sqrt(sn)
        v <- sn - 1
        side <- 1
        if (self$options$a == "0.0050" ||
          self$options$a == "0.0250" ||
          self$options$a == "0.050") {
          side <- 2
        }
        if (side == 1) {
          if (abs(t) > qt(1 - a, v) || abs(t) < qt(a, v)) {
            self$results$conclusion$setTitle("$H_0(\\mu=\\mu_0)$: Reject")
          } else {
            self$results$conclusion$setTitle("$H_0(\\mu=\\mu_0)$: Not Reject")
          }
        } else if (side == 2) {
          if (abs(t) > qt(1 - a, v)) {
            self$results$conclusion$setTitle("$H_0(\\mu=\\mu_0)$: Reject")
          } else {
            self$results$conclusion$setTitle("$H_0(\\mu=\\mu_0)$: Not Reject")
          }
        }
      }
    )
  )
}
