# This file is a generated template, your changes will not be overwritten

TwoSampleTTestEPVClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "TwoSampleTTestEPVClass",
    inherit = TwoSampleTTestEPVBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        s1 <- self$data[, self$options$s1]
        s2 <- self$data[, self$options$s2]
        if (self$options$log) {
          s1 <- log(abs(s1))
          s2 <- log(abs(s2))
        }
        m1 <- mean(s1)
        m2 <- mean(s2)
        n1 <- length(s1)
        n2 <- length(s2)
        a <- as.numeric(self$options$a)
        v <- n1 + n2 - 2
        sd1 <- sd(s1)
        sd2 <- sd(s2)
        sc2 <- ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / v
        t <- (m1 - m2) / sqrt(sc2 * (1 / n1 + 1 / n2))
        side <- 1
        if (self$options$a == "0.0050" ||
          self$options$a == "0.0250" ||
          self$options$a == "0.050") {
          side <- 2
        }
        if (side == 1) {
          if (abs(t) > qt(1 - a, v) || abs(t) < qt(a, v)) {
            self$results$conclusion$setTitle("$H_0(\\mu_1=\\mu_2)$: Reject")
          } else {
            self$results$conclusion$setTitle("$H_0(\\mu_1=\\mu_2)$: Not Reject")
          }
        } else if (side == 2) {
          if (abs(t) > qt(1 - a, v)) {
            self$results$conclusion$setTitle("$H_0(\\mu_1=\\mu_2)$: Reject")
          } else {
            self$results$conclusion$setTitle("$H_0(\\mu_1=\\mu_2)$: Not Reject")
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
