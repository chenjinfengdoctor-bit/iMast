# This file is a generated template, your changes will not be overwritten

SPVSSPClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SPVSSPClass",
    inherit = SPVSSPBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x1 <- self$options$x1
        x2 <- self$options$x2
        n1 <- self$options$n1
        n2 <- self$options$n2
        v <- (x1 + x2) / (n1 + n2)
        a <- as.numeric(self$options$a)
        s <- sqrt(v * (1 - v) * (1 / n1 + 1 / n2))
        p1 <- x1 / n1
        p2 <- x2 / n2
        u <- (p1 - p2) / s
        p <- 1 - pnorm(u)
        dir <- 1
        if (self$options$h1 == "lt") {
          dir <- -dir
        }
        if (self$options$h1 == "neq") {
          p <- 2 * p
        }
        if (p * dir <= a * dir) {
          self$results$h0$setTitle(
            sprintf("$H_0(\\pi_1=\\pi_2)$: Reject")
          )
        } else {
          self$results$h0$setTitle(
            sprintf("$H_0(\\pi_1=\\pi_2)$: Not Reject")
          )
        }
        self$results$h0$setContent(
          sprintf("$u=%f \\\\ P=%f$", u, p)
        )
      }
    )
  )
}
