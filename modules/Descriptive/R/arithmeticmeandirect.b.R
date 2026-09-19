# This file is a generated template, your changes will not be overwritten

ArithmeticMeanDirectClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "ArithmeticMeanDirectClass",
    inherit = ArithmeticMeanDirectBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        df <- self$data[, self$options$x]
        df <- lapply(df, function(x) x[!is.na(x)])
        self$results$mean$setTitle(
          paste0(self$results$mean$title, "$=", mean(unlist(df)), "$")
        )
      }
    )
  )
}

