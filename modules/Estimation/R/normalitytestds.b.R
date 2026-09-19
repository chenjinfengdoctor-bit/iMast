# This file is a generated template, your changes will not be overwritten
library(moments)
ds.test <- function(x) {
  result <- agostino.test(x)
  return(list(
    statistic = result$statistic[2],
    p.value = result$p.value
  ))
}
NormalityTestDSClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "NormalityTestDSClass",
    inherit = NormalityTestDSBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        ds <- ds.test(self$data[, self$options$x])
        self$results$result$setContent(
          c(
            sprintf("statistic = %f", ds$statistic),
            sprintf("p         = %f", ds$p.value)
          )
        )
      }
    )
  )
}
