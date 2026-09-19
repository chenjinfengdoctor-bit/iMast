# This file is a generated template, your changes will not be overwritten
library(ggplot2)
PPPlotClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "PPPlotClass",
    inherit = PPPlotBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        x <- sort(x)
        ecdf <- (rank(x) - 0.5) / length(x)
        tcdf <- pnorm(x, mean(x), sd(x))
        ppd <- data.frame(tcdf = tcdf, ecdf = ecdf)
        self$results$ppp$setState(ppd)
      },
      .ppplot = function(image, ...) {
        plot <- ggplot(image$state, aes(x = ecdf, y = tcdf)) +
          geom_point(shape = 1) +
          geom_abline(
            slope = 1, intercept = 0, color = "gray", linetype = "dashed"
          ) +
          labs(x = "Empirical CDF", y = "Theoretical CDF") +
          theme_minimal()
        print(plot)
        TRUE
      }
    )
  )
}
