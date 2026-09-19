# This file is a generated template, your changes will not be overwritten

SampleSizeContinuousTISClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SampleSizeContinuousTISClass",
    inherit = SampleSizeContinuousTISBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        rdm <- self$options$rdm
        psd <- self$options$psd
        aer <- as.numeric(self$options$aer)
        za <- qnorm(1 - aer)
        ber <- as.numeric(self$options$ber)
        zb <- qnorm(1 - ber)
        n <- 4 * psd^2 / rdm^2 * (za + zb)^2
        self$results$n$setContent(ceiling(n))
      }
    )
  )
}
