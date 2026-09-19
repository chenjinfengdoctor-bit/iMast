# This file is a generated template, your changes will not be overwritten
library(DescTools)
CHMChiSquareTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "CHMChiSquareTestClass",
    inherit = CHMChiSquareTestBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        rc <- self$data[, self$options$rc]
        n <- sum(rc)
        a <- as.numeric(self$options$a)
        h <- self$data[, self$options$h]
        l <- as.array(split(rc, h))
        l <- lapply(l, as.matrix)
        l <- simplify2array(l)
        result <- mantelhaen.test(
          l,
          correct = self$options$correct,
          conf.level = 1 - a
        )
        chi2 <- result$statistic
        p <- result$p.value
        v <- result$parameter
        self$results$h0$setContent(
          c(
            result$method,
            sprintf("$\\chi^2_\\{CMH\\}=%f$", chi2),
            sprintf("$P=%f$", p),
            sprintf("$v=%d$", v),
            sprintf(
              "Confidence Interval:$(%f,%f)$",
              result$conf.int[[1]], result$conf.int[[2]]
            )
          )
        )
        bdr <- BreslowDayTest(l)
        self$results$bdt$setContent(
          c(
            bdr$method,
            sprintf("$\\chi^2=%f$", bdr$statistic),
            sprintf("$v=%d$", bdr$parameter),
            sprintf("$P=%f$", bdr$p.value)
          )
        )
      }
    )
  )
}
