# This file is a generated template, your changes will not be overwritten

PercentileDirectClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "PercentileDirectClass",
    inherit = PercentileDirectBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        X <- unlist(self$data[, self$options$X])
        self$results$px$setTitle(self$results$px$title)
        for (i in 1:99) {
          v <- (length(X) + 1) * i * 0.01
          j <- floor(v)
          g <- v - floor(v)
          px <- X[j]
          if (g == 0) {

          } else {
            px <- (1 - g) * X[j] + g * X[j + 1]
          }
          if (i == self$options$sp) {
            format <- paste0(
              "$P_\\{",
              i,
              "\\}",
              "="
            )
            if (g == 0) {
              format <- paste0(
                format,
                "X_\\{(j)\\}=",
                px,
                "$"
              )
            } else {
              format <- paste0(
                format,
                "(1-g)X_\\{(j)\\}+gX_\\{(j+1)\\}=",
                px,
                "$"
              )
            }
            self$results$px$setTitle(format)
          }
          self$results$pt$addRow(rowKey = i, values = list(
            x = i,
            px = px
          ))
        }
      }
    )
  )
}
