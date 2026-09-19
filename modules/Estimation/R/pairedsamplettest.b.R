# This file is a generated template, your changes will not be overwritten

PairedSampleTTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "PairedSampleTTestClass",
    inherit = PairedSampleTTestBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        d <- self$data[, self$options$s1] - self$data[, self$options$s2]
        n <- length(d)
        sum_d2 <- sum(d * d)
        sum_d <- sum(d)
        mean_d <- mean(d)
        a <- as.numeric(self$options$a)
        sd <- sd(d)
        t <- mean_d / sd * sqrt(n)
        v <- n - 1
        side <- 1
        if (self$options$a == "0.0050" ||
          self$options$a == "0.0250" ||
          self$options$a == "0.050") {
          side <- 2
        }
        if (side == 1) {
          if (abs(t) > qt(1 - a, v) || abs(t) < qt(a, v)) {
            self$results$conclusion$setTitle("$H_0(\\mu_d=0)$: Reject")
          } else {
            self$results$conclusion$setTitle("$H_0(\\mu_d=0)$: Not Reject")
          }
        } else if (side == 2) {
          if (abs(t) > qt(1 - a, v)) {
            self$results$conclusion$setTitle("$H_0(\\mu_d=0)$: Reject")
          } else {
            self$results$conclusion$setTitle("$H_0(\\mu_d=0)$: Not Reject")
          }
        }
        self$results$conclusion$setContent(
          paste0(
            "$P=",
            side * (1 - pt(abs(t), v)),
            "$"
          )
        )
      }
    )
  )
}
