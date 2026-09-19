# This file is a generated template, your changes will not be overwritten

VarianceFrequencyClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "VarianceFrequencyClass",
    inherit = VarianceFrequencyBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        f <- self$data[, self$options$f]
        X <- self$data[, self$options$X]
        sum_f <- sum(f)
        sum_fX <- sum(f * X)
        sum_fX2 <- sum(f * X * X)
        S2 <- (sum_fX2 - (sum_fX^2 / sum_f)) / (sum_f - 1)
        S <- sqrt(S2)
        s2fmt <- "\\frac\\{\\sum\\{fX^2\\}-\\frac\\{(\\sum fX)^2\\}\\{\\sum f\\}\\}\\{\\sum f -1\\}"
        self$results$S2$setTitle(
          paste0(
            self$results$S2$title,
            "$=", s2fmt, "=",
            S2,
            "$"
          )
        )
        self$results$S$setTitle(
          paste0(
            self$results$S$title,
            "$=\\sqrt\\{", s2fmt, "\\}=",
            S,
            "$"
          )
        )
        if (self$options$CV) {
          self$results$CV$setTitle(
            paste0(
              "$CV=\\frac\\{S\\}\\{\\overline X\\} \\times 100\\%=",
              S / (sum_fX / sum_f) * 100,
              "\\%$"
            )
          )
        }
      }
    )
  )
}
