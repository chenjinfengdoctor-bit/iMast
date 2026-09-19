# This file is a generated template, your changes will not be overwritten

SMVSPMClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SMVSPMClass",
    inherit = SMVSPMBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        a <- as.numeric(self$options$a)
        n <- self$options$n
        x <- self$options$x
        p <- self$options$p
        l <- n * p
        px <- 0
        nam <- self$options$nam
        dir <- 1
        if (nam) {
          u <- (x - l) / sqrt(l)
          px <- 1 - pnorm(u)
          if (self$options$h1 == "lt") {
            dir <- -dir
          }
          if (self$options$h1 == "neq") {
            px <- 2 * px
          }
          self$results$h0$setContent(
            sprintf("$u=%f \\\\ P=%f$", u, px)
          )
          if (px <= a) {
            self$results$h0$setTitle(
              sprintf("$H_0(\\pi=%f)$: Reject", p)
            )
          } else {
            self$results$h0$setTitle(
              sprintf("$H_0(\\pi=%f)$: Not Reject", p)
            )
          }
        } else {
          if (self$options$h1 == "gt") {
            px <- 1 - ppois(x - 1, l)
          } else if (self$options$h1 == "lt") {
            px <- ppois(x, l)
          } else if (self$options$h1 == "neq") {
            all_p <- dpois(0:n, l)
            pv <- dpois(x, l)
            px <- sum(all_p[all_p <= pv])
          }
          self$results$h0$setContent(
            sprintf("$P=%f$", px)
          )
        }

        if (dir * px < dir * a) {
          self$results$h0$setTitle(
            sprintf("$H_0(\\pi=%f)$: Reject", p)
          )
        } else {
          self$results$h0$setTitle(
            sprintf("$H_0(\\pi=%f)$: Not Reject", p)
          )
        }
      }
    )
  )
}
