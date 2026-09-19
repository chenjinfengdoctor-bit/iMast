# This file is a generated template, your changes will not be overwritten

SMVSSMNBClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SMVSSMNBClass",
    inherit = SMVSSMNBBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x1 <- na.omit(self$data[, self$options$x1])
        x2 <- na.omit(self$data[, self$options$x2])
        xm1 <- mean(x1)
        xm2 <- mean(x2)
        v1 <- var(x1)
        v2 <- var(x2)
        kc <- (xm1^2 * (v1 - xm1) + xm2^2 * (v2 - xm2)) /
          ((v1 - xm1)^2 + (v2 - xm2)^2)
        y1 <- log(x1 + 0.5 * kc)
        y2 <- log(x2 + 0.5 * kc)
        t <- t.test(y1, y2)$statistic[[1]]
        P <- 2 * (1 - pnorm(t))
        a <- as.numeric(self$options$a)
        if (P <= a) {
          self$results$h0$setTitle(
            "$H_0$: Reject"
          )
        } else {
          self$results$h0$setTitle(
            "$H_0$: Not Reject"
          )
        }
        self$results$h0$setContent(
          sprintf("$t=%f \\\\ P=%f$", t, P)
        )
      }
    )
  )
}
