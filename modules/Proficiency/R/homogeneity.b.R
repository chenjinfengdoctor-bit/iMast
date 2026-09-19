# This file is a generated template, your changes will not be overwritten

homogeneityClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "homogeneityClass",
    inherit = homogeneityBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        #' @export
        x <- self$data[, self$options$target]
        oa <- sum(x) / length(unlist(x))
        m <- length(x)
        rs <- rowSums(x) / m
        g <- nrow(x)
        wt <- abs(x[1] - x[2])
        sx <- sqrt(sum((rs - oa)^2) / (g - 1))
        sw <- sqrt(sum(wt^2) / (2 * g))
        sx2 <- 1 / (g - 1) * sum((rs - oa)^2)
        ss <- sqrt(max(0, sx2 - sw^2 / 2))
        self$results$result$setContent(ss)
      }
    )
  )
}
