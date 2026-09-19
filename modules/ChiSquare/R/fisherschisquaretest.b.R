# This file is a generated template, your changes will not be overwritten

FishersChiSquareTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "FishersChiSquareTestClass",
    inherit = FishersChiSquareTestBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        rc <- self$data[, self$options$rc]
        a <- as.numeric(self$options$a)
        side <- 1
        if (self$options$side == "Two-sided") {
          side <- 2
        }
        alt <- "two.sided"
        if (side == 1) {
          alt <- "greater"
        }
        result <- fisher.test(
          rc,
          conf.level = 1 - a,
          alternative = alt
        )
        P <- result$p.value
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
            result$method,
            sprintf("$P=%f$", result$p.value)
          )
        )
      }
    )
  )
}
