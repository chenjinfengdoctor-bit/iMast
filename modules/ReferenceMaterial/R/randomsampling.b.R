# This file is a generated template, your changes will not be overwritten

RandomSamplingClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "RandomSamplingClass",
    inherit = RandomSamplingBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        csid <- self$data[, self$options$csid]
        csid <- as.character(csid)
        ratio <- self$options$ratio / 100
        set.seed(1)
        r <- sample(csid, as.integer(round(ratio * length(csid))))
        if (self$options$sort) {
          r <- sort(r)
        }
        n <- length(r)
        row_a_col <- 10
        cn <- ceiling(n / row_a_col)
        nc <- min(8, cn)
        rc <- ceiling(n / nc)
        for (i in 1:nc) {
          self$results$t$addColumn(
            name = paste0("i", i),
            title = "ID",
            type = "text"
          )
        }
        for (i in 1:rc) {
          vs <- list()
          for (j in 1:nc) {
            rid <- i + rc * (j - 1)
            if (rid > n) {
              vs[[paste0("i", j)]] <- ""
            } else {
              vs[[paste0("i", j)]] <- r[as.integer(rid)]
            }
          }
          self$results$t$addRow(
            rowKey = i,
            values = vs
          )
        }
      }
    )
  )
}
