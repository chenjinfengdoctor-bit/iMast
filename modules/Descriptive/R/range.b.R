# This file is a generated template, your changes will not be overwritten

RangeClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "RangeClass",
    inherit = RangeBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        df <- self$data[, self$options$X]
        self$results$r$setTitle(
          paste0(
            "$R=",
            max(df, na.rm = TRUE) - min(df, na.rm = TRUE),
            "$"
          )
        )
      }
    )
  )
}
