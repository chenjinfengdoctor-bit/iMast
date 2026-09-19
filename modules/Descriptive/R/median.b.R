# This file is a generated template, your changes will not be overwritten

MedianClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "MedianClass",
    inherit = MedianBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        df <- self$data[, self$options$X]
        df <- lapply(df, function(x) x[!is.na(x)])
        l <- unlist(df)
        m <- median(l)
        format <- "X_\\{(\\frac\\{n+1\\}\\{2\\})\\}="
        if (length(l) %% 2 == 0) {
          format <- "\\frac\\{1\\}\\{2\\}(X_\\{(\\frac\\{n\\}\\{2\\})\\}+X_\\{(\\frac\\{n\\}\\{2\\}+1)\\})="
        }
        self$results$median$setTitle(
          paste0(
            self$results$median$title,
            "$=",
            format,
            m,
            "$"
          )
        )
      }
    )
  )
}
