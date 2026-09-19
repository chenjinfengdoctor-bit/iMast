# This file is a generated template, your changes will not be overwritten

MedianFrequencyClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "MedianFrequencyClass",
    inherit = MedianFrequencyBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        X <- self$data[, self$options$X]
        f <- self$data[, self$options$f]
        X <- unlist(X)
        f <- unlist(f)
        l <- rep(X, f)
        m <- median(l)
        format <- "X_\\{(\\frac\\{n+1\\}\\{2\\})\\}="
        if (length(X) %% 2 == 0) {
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
