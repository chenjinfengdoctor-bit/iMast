# This file is a generated template, your changes will not be overwritten
NormalityTestMomentClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "NormalityTestMomentClass",
    inherit = NormalityTestMomentBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        n <- length(x)
        f <- rep(1, n)
        fx <- f * x
        fx2 <- fx * x
        fx3 <- fx2 * x
        fx4 <- fx3 * x
        g1 <- (n * sum(fx3) - 3 * sum(fx) * sum(fx2) + 2 * sum(fx)^3 / n) /
          ((n - 1) * (n - 2) * ((sum(fx2) - sum(fx)^2 / n) / (n - 1))^1.5)
        g2 <- (n + 1) *
          (n * sum(fx4) - 4 * sum(fx) * sum(fx3) + 6 * sum(fx)^2 * sum(fx2) / n - 3 * sum(fx)^4 / n^2) /
          ((n - 1) * (n - 2) * (n - 3) * ((sum(fx2) - sum(fx)^2 / n) / (n - 1))^2) -
          3 * (n - 1)^2 / ((n - 2) * (n - 3))
        sg1 <- sqrt(6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3)))
        sg2 <- sqrt(24 * n * (n - 1)^2 / ((n - 3) * (n - 2) * (n + 3) * (n + 5)))
        ug1 <- g1 / sg1
        ug2 <- g2 / sg2
        side <- 2
        ps <- side * pnorm(abs(ug1))
        pk <- side * pnorm(abs(ug2))
        ps <- round(ps, 3)
        pk <- round(pk, 3)
        m <- mean(x)
        v <- var(x)
        a <- as.numeric(self$options$a)
        if (ps <= a || pk <= a) {
          self$results$conclusion$setTitle(
            "$H_0(\\gamma_1=\\gamma_2=0)$: Reject"
          )
        } else {
          self$results$conclusion$setTitle(
            "$H_0(\\gamma_1=\\gamma_2=0)$: Not Reject"
          )
        }
        h0 <- "H_0: \\gamma_1=\\gamma_2=0"
        h1 <- "H_1: \\gamma_1 \\ne 0 \\vee \\gamma_2 \\ne 0"
        self$results$conclusion$setContent(
          paste0(
            "$",
            h0, " \\\\", h1, " \\\\",
            "P_\\{kurtosis\\}=", pk,
            " \\\\",
            "P_\\{skewness\\}=", ps,
            "$"
          )
        )
      }
    )
  )
}
