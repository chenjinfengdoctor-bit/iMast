# This file is a generated template, your changes will not be overwritten

TwoWayANOVAWithFactorialDesignClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "TwoWayANOVAWithFactorialDesignClass",
    inherit = TwoWayANOVAWithFactorialDesignBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        v <- self$data[, self$options$value]
        a <- as.character(self$data[, self$options$la])
        b <- as.character(self$data[, self$options$lb])
        vab <- split(v, list(a, b))
        x.m <- lapply(vab, mean)
        x.t <- lapply(vab, sum)
        x.ssq <- lapply(vab, function(x) {
          sum(x^2)
        })
        self$results$text$setContent(x.ssq)
      }
    )
  )
}
