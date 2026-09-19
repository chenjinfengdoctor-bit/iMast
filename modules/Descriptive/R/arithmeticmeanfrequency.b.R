# This file is a generated template, your changes will not be overwritten

ArithmeticMeanFrequencyClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "ArithmeticMeanFrequencyClass",
    inherit = ArithmeticMeanFrequencyBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        X <- self$data[, self$options$X]
        f <- self$data[, self$options$f]
        self$results$mean$setTitle(
          paste0(
            self$results$mean$title, "$=",
            sum(mapply(function(x, y) x * y, X, f)) /
              sum(unlist(f)),
            "$"
          )
        )
      }
    )
  )
}
