# This file is a generated template, your changes will not be overwritten

SMVSSMClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SMVSSMClass",
    inherit = SMVSSMBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x1 <- self$options$x1
        x2 <- self$options$x2
        nneq <- self$options$nneq
        n1 <- self$options$n1
        n2 <- self$options$n2
        u <- 0
        a <- as.numeric(self$options$a)
        if (nneq) {
          n2 <- n1
        }
        if (nneq) {
          if (x1 + x2 >= 20) {
            u <- (x1 - x2) / sqrt(x1 + x2)
          } else if (x1 + x2 > 5) {
            u <- (abs(x1 - x2) - 1) / sqrt(x1 + x2)
          }
        } else {
          if (x1 + x2 >= 20) {
            xm1 <- x1 / n1
            xm2 <- x2 / n2
            u <- (xm1 - xm2) / sqrt(x1 / n1^2 + x2 / n2^2)
          } else if (x1 + x2 > 5) {
            u <- (abs(xm1 - xm2) - 1) / sqrt(x1 / n1^2 + x2 / n2^2)
          }
        }
        P <- 2 * (1 - pnorm(u))
        if (P <= a * 2) {
          self$results$h0$setTitle("$H_0(\\lambda_1=\\lambda_2)$: Reject")
        } else {
          self$results$h0$setTitle("$H_0(\\lambda_1=\\lambda_2)$: Not Reject")
        }
        self$results$h0$setContent(
          sprintf("$u=%f \\\\ P=%f$", u, P)
        )
      }
    )
  )
}
