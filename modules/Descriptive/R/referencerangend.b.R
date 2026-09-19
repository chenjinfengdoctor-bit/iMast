# This file is a generated template, your changes will not be overwritten

ReferenceRangeNDClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "ReferenceRangeNDClass",
    inherit = ReferenceRangeNDBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)

        mean <- self$data[, self$options$mean]
        sd <- self$data[, self$options$sd]
        rr <- self$options$rr
        side <- self$options$side
        utable <- data.frame(
          rr = c("80", "90", "95", "99"),
          os = c(0.84, 1.28, 1.64, 2.33),
          ts = c(1.28, 1.64, 1.96, 2.58)
        )
        u <- utable[which(utable$rr == rr), side]
        for (i in seq_along(mean)) {
          self$results$rrt$addRow(
            rowKey = i,
            values = list(
              rn = i,
              u = u,
              nu = mean[[i]] - u * sd[[i]],
              pu = mean[[i]] + u * sd[[i]]
            )
          )
        }
      }
    )
  )
}
