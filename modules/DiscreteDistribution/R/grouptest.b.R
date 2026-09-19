# This file is a generated template, your changes will not be overwritten

GroupTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "GroupTestClass",
    inherit = GroupTestBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        n <- self$options$n
        m <- self$options$m
        X <- self$options$X
        P <- 1 - (1 - X / n)^(1 / m)
        self$results$P$setTitle(
          sprintf("$P=%.2f\\%%$", P * 100)
        )
        self$results$P$setContent(
          sprintf("$P=1-Q=1-\\sqrt[m]\\{1-\\frac\\{X\\}\\{n\\}\\}=1-\\sqrt[%d]\\{1-\\frac\\{%d\\}\\{%d\\}\\}=%f$", m, X, n, P)
        )
      }
    )
  )
}
