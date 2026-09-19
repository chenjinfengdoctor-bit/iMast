# This file is a generated template, your changes will not be overwritten

QQPlotClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "QQPlotClass",
    inherit = QQPlotBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        x <- sort(x)
        t <- (rank(x) - 0.5) / length(x)
        q <- qnorm(t, mean = mean(x), sd = sd(x))
        self$results$qqp$setState(
          data.frame(x = x, y = q)
        )
      },
      .qqplot = function(image, ...) {
        plot <- ggplot(image$state, aes(x = x, y = y)) +
          geom_point(shape = 1) +
          geom_abline(
            slope = 1, intercept = 0, color = "gray", linetype = "dashed"
          ) +
          labs(x = "Empirical Quantiles", y = "Theoretical Quantiles") +
          theme_minimal()
        print(plot)
        TRUE
      }
    )
  )
}
