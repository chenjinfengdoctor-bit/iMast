# This file is a generated template, your changes will not be overwritten

VarianceClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "VarianceClass",
    inherit = VarianceBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        l <- na.omit(unlist(self$data[, self$options$X]))
        S2 <- var(l)
        S <- sd(l)
        self$results$S2$setTitle(
          paste0(
            self$results$S2$title,
            "$=\\frac\\{\\sum (X-\\overline X)^2\\}\\{n-1\\}=",
            S2,
            "$"
          )
        )
        self$results$S$setTitle(
          paste0(
            self$results$S$title,
            "$=\\sqrt\\{\\frac\\{\\sum (X-\\overline X)^2\\}\\{n-1\\}\\}=",
            S,
            "$"
          )
        )
        if (self$options$CV) {
          self$results$CV$setTitle(
            paste0(
              "$CV=\\frac\\{S\\}\\{\\overline X\\} \\times 100\\%=",
              S / mean(l) * 100,
              "\\%$"
            )
          )
        }
      }
    )
  )
}
