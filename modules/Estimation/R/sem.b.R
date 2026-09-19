# This file is a generated template, your changes will not be overwritten

SEMClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SEMClass",
    inherit = SEMBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        s <- sd(x)
        sx <- s / sqrt(length(x))
        self$results$sem$setContent(sx)
      }
    )
  )
}
