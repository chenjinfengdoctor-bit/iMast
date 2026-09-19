# This file is a generated template, your changes will not be overwritten

SampleSizeCategoricalTIGClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SampleSizeCategoricalTIGClass",
    inherit = SampleSizeCategoricalTIGBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        pc <- self$options$pc
        pe <- self$options$pe
        aer <- as.numeric(self$options$aer)
        za <- qnorm(1 - aer)
        ber <- as.numeric(self$options$ber)
        zb <- qnorm(1 - ber)
        n <- (pc * (1 - pc) + pe * (1 - pe)) / (-pe - pc)^2 * (za + zb)^2
        self$results$n$setContent(ceiling(n))
      }
    )
  )
}
