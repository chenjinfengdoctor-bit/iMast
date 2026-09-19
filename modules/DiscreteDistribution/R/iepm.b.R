# This file is a generated template, your changes will not be overwritten

IEPMClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "IEPMClass",
    inherit = IEPMBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$options$x
        a <- as.numeric(self$options$a)
        l <- 0
        r <- 0
        if (!self$options$nam) {
          l <- qchisq(a, 2 * x) / 2
          r <- qchisq(1 - a, 2 * (x + 1)) / 2
        } else {
          l <- x - qnorm(1 - a) * sqrt(x)
          r <- x + qnorm(1 - a) * sqrt(x)
        }
        self$results$text$setTitle(
          sprintf("$(%f,%f)$", l, r)
        )
      }
    )
  )
}
