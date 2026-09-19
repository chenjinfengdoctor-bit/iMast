# This file is a generated template, your changes will not be overwritten

SampleSizeCategoricalTPSClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SampleSizeCategoricalTPSClass",
    inherit = SampleSizeCategoricalTPSBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        f <- self$options$f
        d <- self$options$d
        aer <- as.numeric(self$options$aer)
        za <- qnorm(1 - aer)
        ber <- as.numeric(self$options$ber)
        zb <- qnorm(1 - ber)
        n <- (za + zb)^2 * f / d^2
        self$results$n$setContent(ceiling(n))
      }
    )
  )
}
