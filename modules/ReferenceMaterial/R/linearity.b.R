# This file is a generated template, your changes will not be overwritten

LinearityClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "LinearityClass",
    inherit = LinearityBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        self$results$abb$setContent(self$results$abb$content)
        hs <- self$options$hs
        ls <- self$options$ls
        l <- as.character(self$data[, self$options$l])
        x <- self$data[, self$options$x]
        y <- self$data[, self$options$y]
        x.g <- split(x, l)
        y.g <- split(y, l)
        x.o <- 0
        y.o <- 0
        for (item in self$options$rc) {
          id <- as.character(item$l)
          v <- x.g[[id]]
          o.g <- grubbs.v(v, self$options$ga)
          if (o.g$n != length(v)) {
            self$results$d$items$og$items$o$setVisible(TRUE)
            x.g[[id]] <- o.g$v
            for (o in o.g$outliers) {
              x.o <- x.o + 1
              self$results$d$items$og$items$o$addRow(
                rowKey = "",
                values = list(mp = "X", l = id, v = o)
              )
            }
          }
          v <- y.g[[id]]
          o.g <- grubbs.v(v, self$options$ga)
          if (o.g$n != length(v)) {
            self$results$d$items$og$items$o$setVisible(TRUE)
            y.g[[id]] <- o.g$v
            for (o in o.g$outliers) {
              y.o <- y.o + 1
              self$results$d$items$og$items$o$addRow(
                rowKey = "",
                values = list(mp = "Y", l = id, v = o)
              )
            }
          }
        }
        if (!self$results$d$items$og$items$o$visible) {
          self$results$d$items$og$items$oc$setContent("Non-declared outliers.")
        } else {
          r <- c()
          if (x.o > 0) {
            r <- c(r, tprintf(
              "There %s %d outlier%s in the sample for measurement procedure X.",
              ifelse(x.o > 1, "were", "was"),
              x.o,
              ifelse(x.o > 1, "s", "")
            ))
          }
          if (y.o > 0) {
            r <- c(r, tprintf(
              "There %s %d outlier%s in the sample for measurement procedure X.",
              ifelse(y.o > 1, "were", "was"),
              y.o,
              ifelse(y.o > 1, "s", "")
            ))
          }
          self$results$d$items$og$items$oc$setContent(r)
        }
        x.r.g <- lapply(x.g, length)
        y.r.g <- lapply(y.g, length)
        x.g.m <- lapply(x.g, mean)
        y.g.m <- lapply(y.g, mean)
        x.g.sd <- lapply(x.g, sd)
        y.g.sd <- lapply(y.g, sd)
        x.g.v <- lapply(x.g, var)
        y.g.v <- lapply(y.g, var)

        v <- list()
        x.y <- list()
        x.w <- list()
        y.y <- list()
        y.w <- list()
        unit <- c()
        if (self$options$m != "") {
          unit <- c(unit, self$options$m)
        }
        if (self$options$unit != "") {
          unit <- c(unit, self$options$unit)
        }
        unit <- paste(unit, collapse = ", ")
        if (unit != "") {
          self$results$d$items$dtx$setTitle(
            sprintf("%s (%s)", self$results$d$items$dtx$title, unit)
          )
          self$results$d$items$dty$setTitle(
            sprintf("%s (%s)", self$results$d$items$dty$title, unit)
          )
          self$results$r$items$mpx$items$dlx$setTitle(
            sprintf("%s (%s)", self$results$r$items$mpx$items$dlx$title, unit)
          )
          self$results$r$items$mpy$items$dly$setTitle(
            sprintf("%s (%s)", self$results$r$items$mpy$items$dly$title, unit)
          )
        }
        for (item in self$options$rc) {
          id <- as.character(item$l)
          v[[id]] <- item$rc * hs + (1 - item$rc) * ls
          self$results$mt$addRow(
            rowKey = id,
            values = list(
              l = as.integer(id),
              rc = as.numeric(item$rc),
              hr = item$rc * 100,
              lr = 100 - item$rc * 100,
              e = v[[id]]
            )
          )
          x.y[[id]] <- x.g.m[[id]]
          y.y[[id]] <- y.g.m[[id]]
          x.w[[id]] <- ifelse(x.g.v[[id]] == 0, 0, x.r.g[[id]] / x.g.v[[id]])
          y.w[[id]] <- ifelse(y.g.v[[id]] == 0, 0, y.r.g[[id]] / y.g.v[[id]])
        }
        if (ls == 0) {
          x.l <- lm(unlist(x.y) ~ unlist(v) + 0, weights = unlist(x.w))
          y.l <- lm(unlist(y.y) ~ unlist(v) + 0, weights = unlist(y.w))
          x.b <- coef(x.l)[1]
          y.b <- coef(y.l)[1]
          x.a <- 0
          y.a <- 0
        } else {
          x.l <- lm(unlist(x.y) ~ unlist(v), weights = unlist(x.w))
          y.l <- lm(unlist(y.y) ~ unlist(v), weights = unlist(y.w))
          x.b <- coef(x.l)[2]
          y.b <- coef(y.l)[2]
          x.a <- coef(x.l)[1]
          y.a <- coef(y.l)[1]
        }
        self$results$r$items$mpx$items$dlx$columns$result$setTitle(
          tprintf(
            "%s ($\\pm %s$%%)",
            self$results$r$items$mpx$items$dlx$columns$result$title,
            ftrim(abs(self$options$adl))
          )
        )
        self$results$r$items$mpy$items$dly$columns$result$setTitle(
          tprintf(
            "%s ($\\pm %s$%%)",
            self$results$r$items$mpy$items$dly$columns$result$title,
            ftrim(abs(self$options$adl))
          )
        )
        n.l.x <- list()
        n.l.y <- list()
        p.x.l <- list()
        p.y.l <- list()
        for (item in self$options$rc) {
          id <- as.character(item$l)
          p.x <- p.x.l[[id]] <- x.b * v[[id]] + x.a
          d.x <- x.y[[id]] - p.x
          dp.x <- d.x / p.x * 100
          p.y <- p.y.l[[id]] <- y.b * v[[id]] + y.a
          d.y <- y.y[[id]] - p.y
          dp.y <- d.y / p.y * 100
          self$results$d$items$dtx$addRow(
            rowKey = id,
            values = list(
              l = id,
              r = x.r.g[[id]],
              m = x.g.m[[id]],
              sd = x.g.sd[[id]],
              cv = 100 * x.g.sd[[id]] / x.g.m[[id]],
              var = x.g.v[[id]],
              w = x.w[[id]]
            )
          )
          self$results$d$items$dty$addRow(
            rowKey = id,
            values = list(
              l = id,
              r = y.r.g[[id]],
              m = y.g.m[[id]],
              sd = y.g.sd[[id]],
              cv = 100 * y.g.sd[[id]] / y.g.m[[id]],
              var = y.g.v[[id]],
              w = y.w[[id]]
            )
          )
          self$results$r$items$mpx$items$dlx$addRow(
            rowKey = id,
            values = list(
              l = id,
              m = x.y[[id]],
              e = v[[id]],
              p = p.x,
              d = d.x,
              dp = dp.x,
              result = ifelse(abs(dp.x) > abs(self$options$adl),
                "No",
                "Yes"
              )
            )
          )
          self$results$r$items$mpy$items$dly$addRow(
            rowKey = id,
            values = list(
              l = id,
              m = y.y[[id]],
              e = v[[id]],
              p = p.y,
              d = d.y,
              dp = dp.y,
              result = ifelse(abs(dp.y) > abs(self$options$adl), "No", "Yes")
            )
          )
          if (abs(dp.x) > abs(self$options$adl)) {
            n.l.x[[id]] <- TRUE
          }
          if (abs(dp.y) > abs(self$options$adl)) {
            n.l.y[[id]] <- TRUE
          }
        }
        self$results$r$items$mpx$items$flx$setState(
          list(
            unit = unit,
            x = unlist(v),
            y = unlist(x.g.m),
            k = x.b,
            b = x.a,
            adl = abs(self$options$adl) / 100
          )
        )
        self$results$r$items$mpy$items$fly$setState(
          list(
            unit = unit,
            x = unlist(v),
            y = unlist(y.g.m),
            k = y.b,
            b = y.a,
            adl = abs(self$options$adl) / 100
          )
        )
        if (ls == 0) {
          self$results$r$items$mpx$items$rx$setContent(tprintf("*The predicted values: $P = %se$", ftrim(x.b)))
          self$results$r$items$mpy$items$ry$setContent(tprintf("*The predicted values: $P = %se$", ftrim(y.b)))
        } else {
          self$results$r$items$mpx$items$rx$setContent(tprintf("*The predicted values: $P = %se %s %s$", ftrim(x.b), ifelse(x.a > 0, "+", ""), ftrim(x.a)))
          self$results$r$items$mpy$items$ry$setContent(tprintf("*The predicted values: $P = %se %s %s$", ftrim(y.b), ifelse(y.a > 0, "+", ""), ftrim(y.a)))
        }
        c <- c("Based on the calculation of the weighted least squares regression, ")
        if (length(n.l.x) > 0 && length(n.l.y) <= 0) {
          c <- c(c, "the linearity test passed on measurement procedure Y. ")
          c <- c(c, tprintf(
            "However, the linearity test did not pass because the measured value of the samples on measurement procedure X exceeded the allowable deviation of linearity. ($\\text{MP}_X$: Level %s)",
            join(names(n.l.x))
          ))
        } else if (length(n.l.x) <= 0 && length(n.l.y) > 0) {
          c <- c(c, "the linearity test passed on measurement procedure X. ")
          c <- c(c, tprintf(
            "the linearity test did not pass because the measured value of the samples on measurement procedure Y exceeded the allowable deviation of linearity. ($\\text{MP}_Y$: Level %s)",
            join(names(n.l.y))
          ))
        } else if (length(n.l.x) > 0 && length(n.l.y) > 0) {
          c <- c(c, tprintf(
            "the linearity test did not pass because the measured value of the samples on both measurement procedures exceeded the allowable deviation of linearity. ($\\text{MP}_X$: Level %s; $\\text{MP}_Y$: Level %s)",
            join(names(n.l.x)), join(names(n.l.y))
          ))
        } else {
          c <- c(c, "the linearity test passed on both measurement procedures.")
        }
        self$results$c$setContent(paste0(c, collapse = ""))
      },
      .linearity.render = function(image, ...) {
        if (is.null(image$state)) {
          return(FALSE)
        }
        xlabel <- "Expected value"
        ylabel <- "Measured value"

        unit <- image$state$unit
        if (unit != "") {
          xlabel <- paste0(xlabel, " (", unit, ")")
          ylabel <- paste0(ylabel, " (", unit, ")")
        }

        df <- data.frame(x = image$state$x, y = image$state$y)
        if (image$state$b == 0) {
          eq_label <- sprintf(
            "Y == %s*italic(x)",
            ifelse(ftrim(image$state$k) == "1", "", ftrim(image$state$k))
          )
        } else {
          eq_label <- sprintf(
            "Y == %s*italic(x)%s%.2f",
            ifelse(ftrim(image$state$k) == "1", "", ftrim(image$state$k)),
            ifelse(image$state$b >= 0, "+ ", "- "),
            abs(image$state$b)
          )
        }
        adl <- data.frame(
          x = custom_breaks(df$x, 100, 0.05),
          reg = image$state$k * custom_breaks(df$x, 100, 0.05) + image$state$b,
          lower = (1 - abs(image$state$adl)) *
            (image$state$k * custom_breaks(df$x, 100, 0.05) + image$state$b),
          upper = (1 + abs(image$state$adl)) *
            (image$state$k * custom_breaks(df$x, 100, 0.05) + image$state$b)
        )
        x_min <- min(range(df$x))
        y_mid <- mean(range(df$y))
        df$group <- "Measured value"

        plot <- ggplot(df, aes(x = x, y = y)) +
          geom_point(aes(color = "Measured value"), shape = 18, size = 5) +
          geom_line(
            data = adl,
            aes(x = x, y = reg, color = "Predicted value"),
            size = 0.35
          ) +
          geom_line(
            data = adl,
            aes(x = x, y = lower, color = "Allowable deviation"),
            size = 0.35,
            linetype = "dashed"
          ) +
          geom_line(
            data = adl,
            aes(x = x, y = upper, color = "Allowable deviation"),
            size = 0.35,
            linetype = "dashed"
          ) +
          annotate("text",
            x = x_min, y = y_mid, label = eq_label, parse = TRUE,
            hjust = -0.1, vjust = -2, size = 5, family = "Times New Roman"
          ) +
          scale_color_manual(
            name = "", values = c(
              "Predicted value" = "black",
              "Measured value" = "#4F81BD",
              "Allowable deviation" = "red"
            )
          ) +
          labs(
            x = xlabel,
            y = ylabel
          ) +
          scale_x_continuous(
            breaks = custom_breaks(df$x, expand.ratio = 0.05),
            limits = custom_breaks(df$x, 2, expand.ratio = 0.05),
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          scale_y_continuous(
            breaks = custom_breaks(df$y, expand.ratio = 0.05),
            limits = custom_breaks(df$y, 2, expand.ratio = 0.05),
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          # expand_limits(x = 0, y = 0) +
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
        TRUE
      }
    )
  )
}
