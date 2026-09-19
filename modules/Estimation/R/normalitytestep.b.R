# This file is a generated template, your changes will not be overwritten

NormalityTestEPClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "NormalityTestEPClass",
    inherit = NormalityTestEPBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        ep <- epps.test(self$data[, self$options$x])
        self$results$result$setContent(
          c(
            sprintf("statistic = %f", ep$statistic),
            sprintf("p         = %f", ep$p.value)
          )
        )
      }
    )
  )
}
