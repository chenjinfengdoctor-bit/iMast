# This file is a generated template, your changes will not be overwritten

McNemarTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "McNemarTestClass",
    inherit = McNemarTestBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        rc <- as.matrix(self$data[, self$options$rc])
        a <- as.numeric(self$options$a)
        assert <- ""
        bc <- rc[2, 1] + rc[1, 2]
        result <- mcnemar.test(rc, correct = self$options$correct)
        P <- result$p.value
        if (bc < 40 && !self$options$correct) {
          assert <-
            sprintf(
              "Note: $b+c=%d<40$, supposed correct checked",
              bc
            )
        }
        self$results$h0$setContent(
          c(
            assert,
            result$method,
            sprintf("$\\chi^2=%f$", result$statistic),
            sprintf("$P=%f$", P),
            sprintf("$v=%d$", result$parameter)
          )
        )
        if (P <= a) {
          self$results$h0$setTitle(
            "$H_0$: Reject"
          )
        } else {
          self$results$h0$setTitle(
            "$H_0$: Not Reject"
          )
        }
      }
    )
  )
}
