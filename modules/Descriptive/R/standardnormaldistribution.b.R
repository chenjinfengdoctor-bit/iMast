# This file is a generated template, your changes will not be overwritten

StandardNormalDistributionClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "StandardNormalDistributionClass",
    inherit = StandardNormalDistributionBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        mean <- self$data[, self$options$mean]
        sd <- self$data[, self$options$sd]
        X <- self$data[, self$options$X]
        for (i in seq_along(mean)) {
          u <- pnorm(X[[i]], mean = mean[[i]], sd = sd[[i]])
          self$results$cdft$addRow(
            rowKey = i,
            values = list(
              mean = mean[[i]],
              sd = sd[[i]],
              X = X[[i]],
              u = u,
              nu = 1 - u
            )
          )
        }
      }
    )
  )
}
