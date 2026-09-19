# This file is a generated template, your changes will not be overwritten

WilcoxonSignedRankTestForTwoIndependentSamplesClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "WilcoxonSignedRankTestForTwoIndependentSamplesClass",
    inherit = WilcoxonSignedRankTestForTwoIndependentSamplesBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x1 <- na.omit(self$data[, self$options$x1])
        x2 <- na.omit(self$data[, self$options$x2])
        n1 <- length(x1)
        n2 <- length(x2)
        a <- as.numeric(self$options$a)
        side <- "two.sided"
        if (self$options$h1 == "lt") {
          side <- "less"
        } else if (self$options$h1 == "gt") {
          side <- "greater"
        }
        result <- wilcox.test(
          x1, x2,
          paired = FALSE,
          alternative = side,
          conf.level = 1 - a,
          correct = self$options$correct
        )
        P <- result$p.value
        self$results$h0$setTitle(
          sprintf("$H_0(X_1=X_2)$: %sReject", ifelse(P <= a, "", "Not "))
        )
        T1 <- result$statistic + n1 * (n1 + 1) / 2
        T2 <- sum(rank(c(x1, x2))) - T1
        self$results$h0$setContent(
          c(
            result$method,
            sprintf("$n_1=%d$", n1),
            sprintf("$n_2=%d$", n2),
            sprintf("$T_1=%f$", T1),
            sprintf("$T_2=%f$", T2),
            sprintf("$P=%f$", P)
          )
        )
      }
    )
  )
}
