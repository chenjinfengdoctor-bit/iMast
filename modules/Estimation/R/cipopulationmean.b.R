# This file is a generated template, your changes will not be overwritten

CIPopulationMeanClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "CIPopulationMeanClass",
    inherit = CIPopulationMeanBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        mean <- self$options$m
        sd <- self$options$sd
        n <- self$options$n
        if (!self$options$s) {
          x <- self$data[, self$options$x]
          mean <- mean(x)
          sd <- sd(x)
          n <- length(x)
        }
        sn <- self$options$sn
        ksd <- self$options$ksd
        psd <- self$options$psd
        cl <- as.numeric(self$options$cl)
        side <- 2
        p <- 1 - cl
        if (cl == 0.05 || cl == 0.01) {
          side <- 1
        }
        l <- 0
        r <- 0
        v <- 0
        if (sn && !ksd) {
          v <- qt(p = p, df = n - 1) * sd / sqrt(n)
        } else {
          if (ksd) {
            sd <- psd
          }
          v <- qnorm(p) * sd / sqrt(n)
        }
        l <- mean - v
        r <- mean + v
        l <- round(l, 3)
        r <- round(r, 3)
        self$results$cl$setTitle(
          paste0("$(", l, ",", r, ")$")
        )
      }
    )
  )
}
