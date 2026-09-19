# This file is a generated template, your changes will not be overwritten

ChiSquareTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "ChiSquareTestClass",
    inherit = ChiSquareTestBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        rc <- self$data[, self$options$rc]
        n <- sum(rc)
        a <- as.numeric(self$options$a)
        result <- chisq.test(rc, correct = self$options$correct)
        chi2 <- result$statistic[[1]]
        P <- result$p.value
        v <- result$parameter
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
          c(
            # result,
            result$method,
            sprintf("$\\chi^2=%f$", chi2),
            sprintf("$P=%f$", P),
            sprintf("$v=%d$", v)
          )
        )
        self$results$T$setContent(
          result$expected
        )
        C <- sqrt(chi2 / (n + chi2))
        self$results$c$setTitle(
          sprintf("$C=%f$", C)
        )
        if (self$options$tlt) {
          x <- seq_len(nrow(rc))
          y <- seq_len(ncol(rc))
          fx <- rowSums(rc)
          fy <- colSums(rc)
          fxy <- 0
          for (r in x) {
            for (c in y) {
              fxy <- fxy + rc[r, c] * x[r] * y[c]
            }
          }
          lxx <- sum(fx * x^2) - sum(fx * x)^2 / sum(fx)
          lyy <- sum(fy * y^2) - sum(fy * y)^2 / sum(fy)
          lxy <- sum(fxy) - sum(fx * x) * sum(fy * y) / n
          b <- lxy / lxx
          sb2 <- lyy / (n * lxx)
          chi2r <- b^2 / sb2
          vr <- 1
          chi2p <- chi2 - chi2r
          vp <- v - vr
          P_r <- 1 - pchisq(chi2r, vr)
          P_p <- 1 - pchisq(chi2p, vp)
          self$results$tlt$setContent(
            c(
              sprintf(
                "$P_\\{linear\\}=%f%s\\alpha$",
                P_r, ifelse(P_r <= a, "\\le", "\\gt")
              ),
              sprintf(
                "$P_\\{partial\\}=%f%s\\alpha$",
                P_p, ifelse(P_p <= a, "\\le", "\\gt")
              )
            )
          )
        }
      }
    )
  )
}
