# This file is a generated template, your changes will not be overwritten

CochranCoxTTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "CochranCoxTTestClass",
    inherit = CochranCoxTTestBase,
    private = list(
      .run = function() {
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

        v1 <- n1 - 1
        v2 <- n2 - 1
        v <- v1 + v2
        if (v1 == v2) {
          v <- v1
        }
        sx1 <- sd1^2 / n1
        sx2 <- sd2^2 / n2
        t <- (m1 - m2) / sqrt(sx1 + sx2)
        a <- as.numeric(self$options$a)
        tav1 <- qt(1 - a, v1)
        tav2 <- qt(1 - a, v2)
        ta <- (sx1 / n1 * tav1 + sx2 / n2 * tav2) /
          (sx1 / n1 + sx2 / n2)
        side <- 1
        if (self$options$a == "0.0050" ||
          self$options$a == "0.0250" ||
          self$options$a == "0.050") {
          side <- 2
        }
        if (side == 1) {
          if (abs(t) > ta || abs(t) < -ta) {
            self$results$conclusion$setTitle("$H_0(\\mu_1=\\mu_2)$: Reject")
          } else {
            self$results$conclusion$setTitle("$H_0(\\mu_1=\\mu_2)$: Not Reject")
          }
        } else if (side == 2) {
          if (abs(t) > ta) {
            self$results$conclusion$setTitle("$H_0(\\mu_1=\\mu_2)$: Reject")
          } else {
            self$results$conclusion$setTitle("$H_0(\\mu_1=\\mu_2)$: Not Reject")
          }
        }
        self$results$conclusion$setContent(
          paste0(
            "$t'=", round(t, 3),
            " \\\\ t'_\\alpha=", round(ta, 3),
            " \\\\ t_\\{\\alpha,v_1\\}=", round(tav1, 3),
            " \\\\ t_\\{\\alpha,v_2\\}=", round(tav2, 3),
            "$"
          )
        )
      }
    )
  )
}
