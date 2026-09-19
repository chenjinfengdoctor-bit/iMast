# This file is a generated template, your changes will not be overwritten
library(deming)
library(mcr)
FittingALineToScatterPlotsClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "FittingALineToScatterPlotsClass",
    inherit = FittingALineToScatterPlotsBase,
    private = list(
      .run = function() {
        wls <- function(x, y, w) {
          model <- lm(y ~ x, weights = w)
          return(list(m = model))
        }
        wdeming <- function(x, y, w, lambda = 1) {
          n <- length(x)
          model <- mcreg(x, y,
            method.reg = "Deming",
            error.ratio = lambda,
            weights = w,
            method.ci = "analtyical"
          )
        }
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        y <- self$data[, self$options$y]
        csd.ols.f <- self$results$csd$items$ols$items$f
        csd.deming.f <- self$results$csd$items$deming$items$f
        ccv.wls.f <- self$results$ccv$items$wls$items$f
        olr.m <- lm(y ~ x)
        olr.s <- coef(olr.m)[2]
        olr.i <- coef(olr.m)[1]
        csd.ols.f$setState(list(
          x = x, y = y, slope = olr.s, intercept = olr.i,
          fit = tprintf(
            "Linear fit (Y = %.2f %s %.2fx)",
            olr.i,
            ifelse(olr.s > 0, "+", ""),
            olr.s
          )
        ))
        deming.m <- deming::deming(y ~ x)
        deming.s <- coef(deming.m)[2]
        deming.i <- coef(deming.m)[1]
        csd.deming.f$setState(list(
          x = x, y = y, slope = deming.s, intercept = deming.i,
          fit = tprintf(
            "Deming fit (Y = %.2f %s %.2fx)",
            deming.i,
            ifelse(deming.s > 0, "+", ""),
            olr.s
          )
        ))
        wls.m <- olr.m
        for (i in 1:5) {
          w <- 1 / (residuals(wls.m)^2 + 1e-8)
          wls.m <- wls(x, y, w)$m
        }
        wls.s <- coef(wls.m)[2]
        wls.i <- coef(wls.m)[1]
        ccv.wls.f$setState(list(
          x = x, y = y, slope = wls.s, intercept = wls.i,
          fit = tprintf(
            "Weighted linear fit (Y = %.2f %s %.2fx)",
            wls.i,
            ifelse(wls.s > 0, "+", ""),
            wls.s
          )
        ))
      },
      .r.render = function(image, ...) {
        if (is.null(image$state)) {
          return(FALSE)
        }
        xlabel <- "Comparative measurement procedure"
        ylabel <- "Candidate measurement procedure"
        df <- data.frame(x = image$state$x, y = image$state$y)
        lines_df <- data.frame(
          slope = c(1, image$state$slope),
          intercept = c(0, image$state$intercept),
          label = c("Identity", image$state$fit)
        )

        range.x <- custom_breaks(c(df$x, df$y))
        range.y <- range.x
        lim.x <- custom_breaks(range.x, lo = 2, expand.ratio = 0)
        lim.y <- custom_breaks(range.y, lo = 2, expand.ratio = 0)
        c.v <- setNames(c("darkgray", "black"), lines_df$label)
        plot <- ggplot(df, aes(x = x, y = y)) +
          scale_x_continuous(
            breaks = range.x,
            limits = lim.x,
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          scale_y_continuous(
            breaks = range.y,
            limits = lim.y,
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          labs(
            x = xlabel,
            y = ylabel
          ) +
          geom_abline(
            data = lines_df,
            aes(slope = slope, intercept = intercept, color = label),
            size = c(0.35, 0.7)
          ) +
          geom_point(color = "#4F81BD", shape = 18, size = 5) +
          scale_color_manual(
            name = "", values = c.v,
            breaks = lines_df$label
          ) +
          theme_minimal() +
          theme(
            plot.margin = unit(c(1, 1, 0.5, 0), "cm"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.line = element_line(color = "black"),
            # legend.position = "right",
            legend.position = c(0.8, 0.2),
            legend.box = "vertical",
            legend.title = element_text(size = 16, family = "Times New Roman"),
            legend.text = element_text(size = 14, family = "Times New Roman"),
            axis.title.x = element_text(size = 16, family = "Times New Roman", margin = margin(t = 10)),
            axis.title.y = element_text(size = 16, family = "Times New Roman", margin = margin(r = 5)),
            axis.text.x = element_text(size = 14, family = "Times New Roman"),
            axis.text.y = element_text(size = 14, family = "Times New Roman")
          )
        print(plot)
        return(TRUE)
      }
    )
  )
}
