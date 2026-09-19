# This file is a generated template, your changes will not be overwritten

KruskalWallisHTestForMultipleIndependentSamplesClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "KruskalWallisHTestForMultipleIndependentSamplesClass",
    inherit = KruskalWallisHTestForMultipleIndependentSamplesBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        a <- as.numeric(self$options$a)

        result <- kruskal.test(x)
        P <- result$p.value
        N <- length(x)
        self$results$h0$setTitle(
          sprintf("$H_0$: %sReject", ifelse(P <= a, "", "Not "))
        )
        self$results$h0$setContent(
          c(
            result$method,
            sprintf("$N=%d$", N),
            sprintf("$H=%f$", result$statistic),
            sprintf("$P=%f$", P)
          )
        )
      }
    )
  )
}
