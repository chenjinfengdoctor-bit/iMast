# This file is a generated template, your changes will not be overwritten
library(PMCMRplus)
NemenyiTestForMultipleIndependentSamplesClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "NemenyiTestForMultipleIndependentSamplesClass",
    inherit = NemenyiTestForMultipleIndependentSamplesBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        g <- factor(rep(names(x), each = nrow(x)))
        a <- as.numeric(self$options$a)
        x <- unlist(x)
        result <- kwAllPairsNemenyiTest(
          x, g,
          dist = "Chisquare"
        )
        pairs <- list()
        for (i in seq_len(ncol(result$p.value))) {
          for (j in 1:i) {
            pairs[paste0(
              colnames(result$p.value)[j], ",", rownames(result$p.value)[i]
            )] <- result$p.value[i, j]
          }
        }
        h0 <- paste(lapply(
          names(pairs), function(k) {
            value <- pairs[[k]]
            sprintf(
              "$H_\\{0(%s)\\}$: %sReject", k,
              ifelse(value <= a, "", "Not ")
            )
          }
        ), collapse = "<br>")
        self$results$h0$setTitle(h0)
        self$results$h0$setContent(c(
          result$method,
          "$\\chi^2$",
          capture.output(print(data.frame(result$statistic))),
          "$P$",
          capture.output(print(data.frame(result$p.value))),
          sprintf("$v=%d$", ncol(x) - 1)
        ))
      }
    )
  )
}
