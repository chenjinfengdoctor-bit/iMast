# This file is a generated template, your changes will not be overwritten

SampleSizeContinuousTPSClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SampleSizeContinuousTPSClass",
    inherit = SampleSizeContinuousTPSBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        sdmd <- self$options$sdmd
        md <- self$options$md
        aer <- as.numeric(self$options$aer)
        za <- qnorm(1 - aer)
        ber <- as.numeric(self$options$ber)
        zb <- qnorm(1 - ber)
        n <- sdmd^2 / md^2 * (za + zb)^2
        self$results$n$setContent(ceiling(n))
      }
    )
  )
}
