# This file is a generated template, your changes will not be overwritten

WilcoxonSignedRankTestForPairedSampleFrequencyClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "WilcoxonSignedRankTestForPairedSampleFrequencyClass",
    inherit = WilcoxonSignedRankTestForPairedSampleFrequencyBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x1 <- self$data[, self$options$x1]
        x2 <- self$data[, self$options$x2]
        a <- as.numeric(self$options$a)
        x1x2 <- x1 + x2
        ritem <- rep(seq_along(x1x2), x1x2)
        avgr <- unique(rank(ritem))
        x1r <- avgr * x1
        x2r <- avgr * x2
        n1 <- sum(x1)
        n2 <- sum(x2)
        T1 <- sum(x1r)
        T2 <- sum(x2r)
        T <- T1
        n <- n1
        if (n1 > n2) {
          T <- T2
          n <- n2
        }
        N <- n1 + n2
        value <- sum(x1x2^3 - x1x2)
        u <- (T - n * (N + 1) / 2) /
          sqrt(n1 * n2 * (N + 1) / 12 * (1 - value / (N^3 - N)))
        side <- 2
        if (self$options$h1 == "lt") {
          side <- 1
        } else if (self$options$h1 == "gt") {
          side <- 1
        }
        P <- side * (1 - pnorm(u))
        self$results$h0$setTitle(
          sprintf(
            "$H_0(X_1=X_2)$: %sReject",
            ifelse(P <= a, "", "Not ")
          )
        )
        self$results$h0$setContent(
          c(
            sprintf("$n_1=%d$", n1),
            sprintf("$n_2=%d$", n2),
            sprintf("$T_1=%f$", T1),
            sprintf("$T_2=%f$", T2),
            sprintf("$u=%f$", u),
            sprintf("$P=%f$", P)
          )
        )
      }
    )
  )
}
