# This file is a generated template, your changes will not be overwritten

BinomialDistributionProbabilityClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "BinomialDistributionProbabilityClass",
    inherit = BinomialDistributionProbabilityBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        self$results$p$setTitle(
          sprintf(
            "$P(%d)=%f$",
            self$options$x,
            dbinom(
              self$options$x,
              self$options$n,
              self$options$p
            )
          )
        )
      }
    )
  )
}
