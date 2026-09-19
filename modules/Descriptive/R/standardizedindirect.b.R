# This file is a generated template, your changes will not be overwritten

StandardizedIndirectClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "StandardizedIndirectClass",
    inherit = StandardizedIndirectBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        na <- self$data[, self$options$na]
        nb <- self$data[, self$options$nb]
        p <- self$data[, self$options$p]
        ra <- self$options$ra
        rb <- self$options$rb
        P <- (ra + rb) / sum(na + nb)
        npa <- na * p
        npb <- nb * p
        smr_a <- ra / sum(npa)
        smr_b <- rb / sum(npb)
        ppa <- P * smr_a
        ppb <- P * smr_b
        self$results$ppa$setContent(ppa)
        self$results$ppb$setContent(ppb)
      }
    )
  )
}
