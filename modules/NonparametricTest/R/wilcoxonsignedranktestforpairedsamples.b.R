# This file is a generated template, your changes will not be overwritten

WilcoxonSignedRankTestForPairedSamplesClass <-
  if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
      "WilcoxonSignedRankTestForPairedSamplesClass",
      inherit = WilcoxonSignedRankTestForPairedSamplesBase,
      private = list(
        .run = function() {
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          x1 <- self$data[, self$options$x1]
          x2 <- self$data[, self$options$x2]
          n <- sum((x1 - x2) != 0)
          a <- as.numeric(self$options$a)
          result <- wilcox.test(
            x1, x2,
            paired = TRUE,
            conf.level = 1 - a,
            correct = self$options$correct
          )
          P <- result$p.value
          self$results$h0$setTitle(
            sprintf("$H_0(M_d=0)$: %sReject", ifelse(P <= a, "", "Not "))
          )
          self$results$h0$setContent(
            c(
              result$method,
              sprintf("$n=%d$", n),
              sprintf("$T=%f$", result$statistic),
              sprintf("$P=%f$", P)
            )
          )
        }
      )
    )
  }
