# This file is a generated template, your changes will not be overwritten

NormalityTestADClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "NormalityTestADClass",
    inherit = NormalityTestADBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        ad <- ad.test(self$data[, self$options$x])
        self$results$result$setContent(
          c(
            sprintf("statistic = %f", ad$statistic),
            sprintf("p         = %f", ad$p.value)
          )
        )
      }
    )
  )
}
