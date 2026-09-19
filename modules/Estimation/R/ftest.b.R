# This file is a generated template, your changes will not be overwritten

FTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "FTestClass",
    inherit = FTestBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        var1 <- self$options$var1
        var2 <- self$options$var2
        v1 <- self$options$n1 - 1
        v2 <- self$options$n2 - 1
        if (self$options$raw) {
          x1 <- na.omit(self$data[, self$options$x1])
          x2 <- na.omit(self$data[, self$options$x2])
          var1 <- var(x1)
          var2 <- var(x2)
          v1 <- length(x1) - 1
          v2 <- length(x2) - 1
        }

        if (var1 < var2) {
          var_temp <- var2
          var2 <- var1
          var1 <- var_temp
          v_temp <- v2
          v2 <- v1
          v1 <- v_temp
        }
        f <- var1 / var2
        side <- 2
        p <- side * (1 - pf(f, v1, v2))
        p <- round(p, 3)
        a <- as.numeric(self$options$a)
        qf <- qf(1 - a, v1, v2)
        if (f > qf) {
          self$results$conclusion$setTitle(
            "$H_0(\\sigma_1^2=\\sigma_2^2)$: Reject"
          )
        } else {
          self$results$conclusion$setTitle(
            "$H_0(\\sigma_1^2=\\sigma_2^2)$: Not Reject"
          )
        }
        self$results$conclusion$setContent(
          paste0(
            "$",
            "F=", round(f, 3),
            " \\\\",
            "P=", p,
            " \\\\",
            sprintf(
              "F_\\{%.2f,(%d,%d)\\}=%f",
              a, v1, v2, qf
            ),
            "$"
          )
        )
      }
    )
  )
}
