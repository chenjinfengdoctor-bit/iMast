# This file is a generated template, your changes will not be overwritten

SPVSPPClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SPVSPPClass",
    inherit = SPVSPPBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        a <- as.numeric(self$options$a)
        n <- self$options$n
        x <- self$options$x
        p <- self$options$p
        px <- 0
        nam <- self$options$nam
        dir <- 1
        if (nam) {
          pp <- x / n
          u <- (pp - p) / sqrt(p * (1 - p) / n)
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
            px <- 1 - pbinom(x - 1, size = n, prob = p)
          } else if (self$options$h1 == "lt") {
            px <- pbinom(x, size = n, prob = p)
          } else if (self$options$h1 == "neq") {
            all_p <- dbinom(0:n, size = n, prob = p)
            pv <- dbinom(x, size = n, prob = p)
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
