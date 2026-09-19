# This file is a generated template, your changes will not be overwritten

StandardizedClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "StandardizedClass",
    inherit = StandardizedBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        da <- self$data[, self$options$da]
        na <- self$data[, self$options$na]
        db <- self$data[, self$options$db]
        nb <- self$data[, self$options$nb]
        ppa <- 0
        ppb <- 0
        if (self$options$sm == "dn") {
          n <- da + db
          N <- sum(n)
          pa <- na / da
          pb <- nb / db
          npa <- n * pa
          npb <- n * pb
          ppa <- sum(npa) / N
          ppb <- sum(npb) / N
        } else if (self$options$sm == "dr") {
          n <- da + db
          N <- sum(n)
          rn <- n / N
          pa <- na / da
          pb <- nb / db
          npa <- rn * pa
          npb <- rn * pb
          ppa <- sum(npa)
          ppb <- sum(npb)
        }
        self$results$ppa$setContent(ppa)
        self$results$ppb$setContent(ppb)
      }
    )
  )
}
