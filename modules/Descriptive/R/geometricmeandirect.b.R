# This file is a generated template, your changes will not be overwritten

GeometricMeanDirectClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "GeometricMeanDirectClass",
    inherit = GeometricMeanDirectBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        df <- self$data[, self$options$X]
        df <- lapply(df, function(x) x[!is.na(x)])
        df <- unlist(df)
        self$results$gmean$setTitle(
          paste0(
            self$results$gmean$title,
            "$=",
            10^(sum(log10(df)) / length(df)),
            "$"
          )
        )
      }
    )
  )
}
