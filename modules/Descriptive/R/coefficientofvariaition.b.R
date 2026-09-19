# This file is a generated template, your changes will not be overwritten

CoefficientOfVariaitionClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "CoefficientOfVariaitionClass",
    inherit = CoefficientOfVariaitionBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        S <- self$data[, self$options$sd]
        X <- self$data[, self$options$mean]
        cv <- S / X * 100
        for (i in seq_along(cv)) {
          self$results$cvt$addRow(
            rowKey = i,
            values = list(
              rn = as.integer(i),
              cv = cv[[i]]
            )
          )
        }
      }
    )
  )
}
