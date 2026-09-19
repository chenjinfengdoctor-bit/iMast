# This file is a generated template, your changes will not be overwritten
library(ggplot2)
MinCrashClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "MinCrashClass",
    inherit = MinCrashBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        l <- 100
        dx <- seq(1:l)
        dy <- seq(from = l, to = 1)
        lm <- lm(dy ~ dx)
        self$results$im$setState(list(
          x = dx, y = dy, lm = lm
        ))
        for (i in seq(1:l)) {
          self$results$t$addRow(
            rowKey = i,
            values = list(
              c1 = "c1",
              c2 = "c2",
              c3 = "c3",
              c4 = "c4"
            )
          )
        }
      },
      .render = function(image, ...) {
        df <- data.frame(x = image$state$x, y = image$state$y)
        coefs <- coef(image$state$lm)
        k <- coefs[2]
        x_mid <- mean(range(df$x))
        y_mid <- mean(range(df$y))
        df$group <- "Values"
        plot <- ggplot(df, aes(x = x, y = y)) +
          geom_point(aes(color = "Values"), data = df, shape = 18, size = 5) +
          annotate("text",
            x = x_mid, y = y_mid, label = eq_label,
            hjust = 0, vjust = 1.5, size = 5, family = "Times New Roman"
          )
        print(plot)
        TRUE
      }
    )
  )
}
