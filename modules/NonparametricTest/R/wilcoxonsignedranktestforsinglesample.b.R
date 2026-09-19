# This file is a generated template, your changes will not be overwritten

WilcoxonSignedRankTestForSingleSampleClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "WilcoxonSignedRankTestForSingleSampleClass",
    inherit = WilcoxonSignedRankTestForSingleSampleBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        m0 <- self$options$m0
        xd <- x * 1000 - m0 * 1000
        n <- sum(xd != 0)
        a <- as.numeric(self$options$a)
        side <- "two.sided"
        if (self$options$h1 == "lt") {
          side <- "less"
        } else if (self$options$h1 == "gt") {
          side <- "greater"
        }
        result <- wilcox.test(
          xd,
          conf.level = 1 - a,
          alternative = side,
          correct = self$options$correct
        )
        P <- result$p.value
        self$results$h0$setTitle(
          sprintf("$H_0(M=M_0)$: %sReject", ifelse(P <= a, "", "Not "))
        )
        self$results$h0$setContent(
          c(
            result$method,
            sprintf("$n=%d$", n),
            sprintf("$T=%f$", result$statistic),
            sprintf("$P=%f$", P)
          )
        )
      }
    )
  )
}
