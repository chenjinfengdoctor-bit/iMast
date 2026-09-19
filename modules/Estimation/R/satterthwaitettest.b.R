# This file is a generated template, your changes will not be overwritten

SatterthwaiteTTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SatterthwaiteTTestClass",
    inherit = SatterthwaiteTTestBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        m1 <- self$options$m1
        m2 <- self$options$m2
        n1 <- self$options$n1
        n2 <- self$options$n2
        sd1 <- self$options$sd1
        sd2 <- self$options$sd2
        if (self$options$raw) {
          s1 <- self$data[, self$options$s1]
          s2 <- self$data[, self$options$s2]
          m1 <- mean(s1)
          m2 <- mean(s2)
          n1 <- length(s1)
          n2 <- length(s2)
          sd1 <- sd(s1)
          sd2 <- sd(s2)
        }
        sx1 <- sd1^2 / n1
        sx2 <- sd2^2 / n2
        v <- (sx1 + sx2)^2 / (sx1^2 / (n1 - 1) + sx2^2 / (n2 - 1))
        vraw <- v
        v <- round(v)
        t <- (m1 - m2) / sqrt(sx1 + sx2)
        a <- as.numeric(self$options$a)
        side <- 1
        if (self$options$a == "0.0050" ||
          self$options$a == "0.0250" ||
          self$options$a == "0.050") {
          side <- 2
        }
        p <- side * (1 - pt(abs(t), v))
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
            "$t=", round(t, 3),
            "\\\\ v=", round(vraw, 3), "\\approx", v,
            "\\\\ P=", round(side * (1 - pt(abs(t), v)), 3),
            "$"
          )
        )
      }
    )
  )
}
