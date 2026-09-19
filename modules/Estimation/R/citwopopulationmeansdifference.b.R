# This file is a generated template, your changes will not be overwritten

CITwoPopulationMeansDifferenceClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "CITwoPopulationMeansDifferenceClass",
    inherit = CITwoPopulationMeansDifferenceBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        n1 <- self$options$n1
        m1 <- self$options$m1
        sd1 <- self$options$sd1

        n2 <- self$options$n2
        m2 <- self$options$m2
        sd2 <- self$options$sd2
        cl <- as.numeric(self$options$cl)
        p <- 1 - cl
        v <- n1 + n2 - 2
        sn <- self$options$sn
        d <- 0
        sc2 <- ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / v
        sed2m <- sqrt(sc2 * (1 / n1 + 1 / n2))
        if (!sn) {
          d <- qnorm(p) * sed2m
        } else {
          if (self$options$sed2m) {
            sed2m <- sqrt(sd1^2 / n1 + sd2^2 / n2)
          }
          d <- qt(p = p, df = v) * sed2m
        }
        l <- m1 - m2 - d
        r <- m1 - m2 + d
        l <- round(l, 3)
        r <- round(r, 3)

        self$results$cl$setTitle(
          paste0(
            "$(", l, ",", r, ")$"
          )
        )
      }
    )
  )
}
