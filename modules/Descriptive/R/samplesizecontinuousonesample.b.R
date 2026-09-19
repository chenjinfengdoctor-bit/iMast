# This file is a generated template, your changes will not be overwritten

SampleSizeContinuousOneSampleClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SampleSizeContinuousOneSampleClass",
    inherit = SampleSizeContinuousOneSampleBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        dd <- self$options$dd
        psd <- self$options$psd
        aer <- as.numeric(self$options$aer)
        za <- qnorm(1 - aer)
        ber <- as.numeric(self$options$ber)
        zb <- qnorm(1 - ber)
        n <- psd^2 / dd^2 * (za + zb)^2
        self$results$n$setContent(ceiling(n))
      }
    )
  )
}
