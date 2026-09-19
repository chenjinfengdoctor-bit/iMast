# This file is a generated template, your changes will not be overwritten

IEPPClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "IEPPClass",
    inherit = IEPPBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        a <- as.numeric(self$options$a)
        x <- self$options$x
        n <- self$options$n
        l <- 0
        r <- 0
        if (!self$options$nam) {
          tr <- binom.test(x, n, conf.level = 1 - a)
          l <- tr$conf.int[1]
          r <- tr$conf.int[2]
        } else {
          ua <- qnorm(1 - a)
          p <- x / n
          sp <- sqrt(p * (1 - p) / 100)
          l <- p - ua * sp
          r <- p + ua * sp
        }
        self$results$text$setTitle(
          sprintf("$(%.2f\\%%,%.2f\\%%)$", l * 100, r * 100)
        )
      }
    )
  )
}
