# This file is a generated template, your changes will not be overwritten

SampleSizeCategoricalOneSampleClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SampleSizeCategoricalOneSampleClass",
    inherit = SampleSizeCategoricalOneSampleBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        p0 <- self$options$p0
        p1 <- self$options$p1
        aer <- as.numeric(self$options$aer)
        za <- qnorm(1 - aer)
        ber <- as.numeric(self$options$ber)
        zb <- qnorm(1 - ber)
        n <- (za * sqrt(p0 * (1 - p0)) + zb * sqrt(p1*(1 - p1)))^2 /
          (p1 - p0)^2
        self$results$n$setContent(ceiling(n))
      }
    )
  )
}
