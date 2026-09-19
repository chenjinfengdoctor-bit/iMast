# This file is a generated template, your changes will not be overwritten

SampleSizeForISTClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SampleSizeForISTClass",
    inherit = SampleSizeForISTBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        if (self$options$side == "o") {
          za <- qnorm(1 - self$options$alpha)
        } else {
          za <- qnorm(1 - self$options$alpha / 2)
        }
        zb <- qnorm(1 - self$options$power)
        N <- 2 * (((za + zb) * self$options$precision) / self$options$diff)^2
        t <- self$results$t
        t$addRow(
          rowKey = 1,
          values = list(
            t1 = "Type I Error $\\alpha$",
            v1 = ftrim(self$options$alpha),
            t2 = "Type II Error $\\beta$",
            v2 = ftrim(self$options$power)
          )
        )
        t$addRow(
          rowKey = 2,
          values = list(
            t1 = tprintf(
              "$Z_{1-\\alpha%s}$",
              ifelse(self$options$side == "o", "", "/2")
            ),
            v1 = ftrim(za),
            t2 = tprintf("$Z_{1-\\beta}$"),
            v2 = ftrim(zb)
          )
        )
        t$addRow(
          rowKey = 3,
          values = list(
            t1 = "$\\delta$",
            v1 = ftrim(self$options$diff),
            t2 = "$\\sigma$",
            v2 = ftrim(self$options$precision)
          )
        )
        t$addRow(
          rowKey = 4,
          values = list(
            t1 = "$N_T$",
            v1 = ftrim(ceiling(N)),
            t2 = "$N_C$",
            v2 = ftrim(ceiling(N))
          )
        )
        self$results$text$setContent(
          c(
            tprintf(
              "Number of replicates $N \\ge 2 [(Z_{1-\\alpha%s}+Z_{1-\\beta})\\sigma/\\delta]^2 = %s \\approx %d$",
              ifelse(self$options$side == "o", "", "/2"), ftrim(N), ceiling(N)
            ),
            tprintf(
              "Number of replicates required for the test sample ($N_T$) and number of replicates required for the control sample ($N_C$) should be %d.",
              ceiling(N)
            )
          )
        )
      }
    )
  )
}
