# This file is a generated template, your changes will not be overwritten

GeometricMeanFrequencyClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "GeometricMeanFrequencyClass",
    inherit = GeometricMeanFrequencyBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        X <- self$data[, self$options$X]
        f <- self$data[, self$options$f]
        self$results$gmean$setTitle(
          paste0(
            self$results$gmean$title, "$=",
            10^(
              sum(mapply(function(x, f) log10(x) * f, X, f))
              / sum(unlist(f))
            ),
            "$"
          )
        )
      }
    )
  )
}
