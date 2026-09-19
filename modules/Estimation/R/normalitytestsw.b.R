# This file is a generated template, your changes will not be overwritten
sw.test <- function(x) {
  result <- shapiro.test(x)
  return(list(
    statistic = result$statistic,
    p.value = result$p.value
  ))
}

NormalityTestSWClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "NormalityTestSWClass",
    inherit = NormalityTestSWBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        sw <- sw.test(self$data[, self$options$x])
        self$results$result$setContent(
          c(
            sprintf("statistic = %f", sw$statistic),
            sprintf("p         = %f", sw$p.value)
          )
        )
      }
    )
  )
}
