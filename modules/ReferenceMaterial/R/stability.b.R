# This file is a generated template, your changes will not be overwritten

StabilityClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "StabilityClass",
    inherit = StabilityBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        y <- self$data[, self$options$y]
        x <- self$data[, self$options$x]
        a <- self$options$alpha
        x.m <- mean(x)
        y.m <- rowMeans(y)
        y.m.m <- mean(y.m)
        n <- length(x)
        data <- data.frame(x = x, y = y.m)
        b1 <- sum(apply(data, 1, function(row) {
          (row[["x"]] - x.m) * (row[["y"]] - y.m.m)
        })) / sum(apply(data, 1, function(row) {
          (row[["x"]] - x.m)^2
        }))
        b0 <- y.m.m - b1 * x.m
        s <- sqrt(sum((y.m - b0 - b1 * x)^2) / (n - 2))
        s_b1 <- s / sqrt(sum((data$x - x.m)^2))
        sig <- abs(b1) < qt(1 - a/2, n - 2) * s_b1
        pd <- data.frame(x = rep(x, ncol(y)), y = unlist(y))
        self$results$text$setContent(
          c(
            tprintf("Number of time: $n =%d$", n),
            tprintf("Slope: $b_1$ = %s", sp(b1)),
            tprintf("Intercept: $b_0 = $ %.4f", b0),
            tprintf("Stand Error: $s = $ %s", sp(s)),
            tprintf("Stand Error of slope: $s(b_1) = $ %s", sp(s_b1)),
            tprintf(
              "Since $|b_1| %s t_{%s,n-2} \\times s(b_1) = $ %s with $t_{%s,n-2} = %s$, the slope can%s be considered as zero.",
              ifelse(sig, "\\lt", "\\ge"),
              ftrim(1 - a/2),
              sp(qt(1 - a/2, n - 2) * s_b1),
              ftrim(1 - a/2),
              sp(qt(1 - a/2, n - 2)),
              ifelse(sig, "", " not")
            )
          )
        )
        self$results$f$setState(
          list(point.data = pd, line.data = data, xlabel = "Time point number", ylabel = "control materials", p = 1)
        )
      },
      .ols.render = function(image, ...) {
        if (is.null(image$state)) {
          return(FALSE)
        }
        fd <- image$state$line.data
        pd <- image$state$point.data
        lm <- lm(y ~ x, data = fd)
        xlabel <- image$state$xlabel
        ylabel <- image$state$ylabel
        range.x <- c(min(pd$x), max(pd$x))
        range.y <- c(min(pd$y), max(pd$y))
        scml <- list("Regression line" = "#070606")
        scml[[image$state$ylabel]] <- "#4F81BD"
        if (image$state$p == 2) {
          fml <- y~x + I(x^2)
        } else {
          fml <- y ~ x
        }
        plot <- ggplot(data = fd, aes(x = x, y = y)) +
          geom_smooth(
            method = "lm",
            formula = fml,
            level = 1 - self$options$alpha,
            se = TRUE, aes(color = "Regression line"),
            size = 0.35
          ) +
          geom_point(data = pd, aes(color = image$state$ylabel), shape = 18, size = 5) +
          labs(
            x = xlabel,
            y = ylabel
          ) +
          scale_color_manual(
            name = "", values = unlist(scml)
          ) +
          scale_x_continuous(
            breaks = fd$x,
            limits = custom_breaks(range.x, 2),
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          scale_y_continuous(
            breaks = custom_breaks(range.y, expand.ratio = 0.7),
            limits = custom_breaks(range.y, 2, expand.ratio = 0.7),
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          theme_minimal() +
          theme(
            plot.margin = unit(c(0.7, 0.7, 0.7, 0.7), "cm"),
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
