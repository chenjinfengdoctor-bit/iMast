# This file is a generated template, your changes will not be overwritten

RelativeClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "RelativeClass",
    inherit = RelativeBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        X <- self$data[, self$options$X]
        name <- seq_along(X)
        if (!is.null(self$options$name)) {
          name <- self$data[, self$options$name]
        }
        total <- self$options$st
        if (total <= 0) {
          total <- sum(X)
        }
        base <- as.integer(self$options$base)
        sum_sr <- 0

        rr1r <- NULL
        rr2r <- NULL

        for (i in seq_along(X)) {
          v <- X[[i]] / total * base
          if (!is.null(self$options$rr1) && name[[i]] == self$options$rr1) {
            rr1r <- v
          }
          if (!is.null(self$options$rr2) && name[[i]] == self$options$rr2) {
            rr2r <- v
          }
          self$results$rt$addRow(
            rowKey = name[[i]],
            values = list(
              c = name[[i]],
              number = X[[i]],
              strength = v,
              structural = v
            )
          )
          sum_sr <- sum_sr + v
        }

        self$results$rt$addRow(
          rowKey = "total",
          values = list(
            c = "Total",
            number = sum(X),
            strength = "-",
            structural = sum_sr
          )
        )
        if (!is.null(rr1r) && !is.null(rr2r)) {
          self$results$rr$setContent(rr1r / rr2r * base)
        }
      }
    )
  )
}
