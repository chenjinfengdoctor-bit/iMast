# This file is a generated template, your changes will not be overwritten

EstimatingBiasFromDifferencePlotClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "EstimatingBiasFromDifferencePlotClass",
    inherit = EstimatingBiasFromDifferencePlotBase,
    private = list(
      .run = function() {
        esd <- function(d, alpha = 0.05) {
          N <- length(d)
          i <- 0
          o.d <- d
          outlier_idx <- vector()
          esd <- vector()
          m <- vector()
          lambda <- vector()
          sd <- vector()
          d.v <- vector()
          while (TRUE) {
            i <- i + 1
            d.m <- mean(o.d)
            d.sd <- sd(o.d)
            max_j <- 1
            for (j in seq_along(o.d)) {
              if (abs(o.d[[j]] - d.m) / d.sd > abs(o.d[[max_j]] - d.m) / d.sd) {
                max_j <- j
              }
            }
            d.esd <- abs(o.d[[max_j]] - d.m) / d.sd
            p <- 1 - alpha / 2 / (N - i + 1)
            v <- N - i - 1
            tvp <- qt(p, v)
            lambda.i <- tvp * (N - i) / sqrt((N - i + 1) * (v + tvp^2))
            if (d.esd > lambda.i) {
              outlier_idx <- append(outlier_idx, max_j)

              esd <- append(esd, d.esd)
              m <- append(m, d.m)
              lambda <- append(lambda, lambda.i)
              sd <- append(sd, d.sd)
              d.v <- append(d.v, o.d[max_j])

              o.d <- o.d[-max_j]

              next
            } else {
              break
            }
          }
          return(
            list(
              o.d = o.d, o.i = outlier_idx, esd = esd,
              m = m, lambda = lambda, sd = sd, d.v = d.v
            )
          )
        }
        wilcoxon_ci <- function(d, alpha = 0.05) {
          n <- length(d)
          pairs <- t(combn(n, 2, simplify = TRUE))
          avg_pairs <- (d[pairs[, 1]] + d[pairs[, 2]]) / 2
          diag_vals <- (d + d) / 2
          all_vals <- c(avg_pairs, diag_vals)
          mid <- median(all_vals)
          ca <- n * (n + 1) / 2 + 1 - qsignrank(alpha / 2, n)
          return(list(
            mid = mid,
            lower = all_vals[[ca]],
            upper = all_vals[[qsignrank(alpha / 2, n)]]
          ))
        }
        median_ci <- function(d, alpha = 0.05) {
          sorted_d <- sort(d)
          N <- length(sorted_d)
          if (N %% 2 == 0) {
            median_est <- (sorted_d[N / 2] + sorted_d[N / 2 + 1]) / 2
            median_pos <- (N / 2 + 0.5)
          } else {
            median_est <- sorted_d[(N + 1) / 2]
            median_pos <- (N + 1) / 2
          }
          z_alpha <- qnorm(1 - alpha / 2)
          offset <- 0.5 * sqrt(N) * z_alpha
          lower_pos <- floor(median_pos - offset)
          upper_pos <- ceiling(median_pos + offset)
          lower_pos <- max(1, lower_pos)
          upper_pos <- min(N, upper_pos)
          lower_ci <- sorted_d[lower_pos]
          upper_ci <- sorted_d[upper_pos]
          list(
            median = median_est,
            conf.int = c(lower_ci, upper_ci),
            positions = c(lower_pos, upper_pos)
          )
        }
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        y <- self$data[, self$options$y]
        alpha <- self$options$alpha
        df <- self$results$dp$items$d
        dhf <- self$results$ddp$items$dh
        pdf <- self$results$dp$items$pd
        pdhf <- self$results$ddp$items$pdh
        dpc <- self$results$dp$items$c
        ddpc <- self$results$ddp$items$c
        t <- self$results$t
        added.pct <- c(
          "cil", "ciu", "cimidl", "cimidu", "citukeyl", "citukeyu"
        )
        for (pct in added.pct) {
          t$columns[[pct]]$setSuperTitle(
            tprintf(
              "%s%% %s", ftrim(100 - 100 * alpha),
              t$columns[[pct]]$superTitle
            )
          )
        }

        c <- self$results$c
        d <- y - x
        pd <- d / (y + x) * 2 * 100
        d.x <- x
        d.y <- y
        pd.x <- x
        pd.y <- y
        oc <- self$results$o$items$c
        oc.r <- c(
          tprintf("Non-declared outliers of difference."),
          tprintf("Non-declared outliers of percent difference.")
        )
        if (self$options$ho) {
          odt <- self$results$o$items$od
          d.o.result <- esd(d, alpha)
          for (i in seq_along(d.o.result$d.v)) {
            odt$addRow(
              rowKey = i,
              values = list(
                x = d.x[[d.o.result$o.i[[i]]]],
                y = d.y[[d.o.result$o.i[[i]]]],
                b = d.o.result$d.v[[i]],
                mean = d.o.result$m[[i]],
                esd = d.o.result$esd[[i]],
                sd = d.o.result$sd[[i]],
                lambda = d.o.result$lambda[[i]]
              )
            )
          }
          if (length(d.o.result$o.i) > 0) {
            odt$setVisible(TRUE)
            oc.r[[1]] <- tprintf(
              "According to the extreme studentized deviate test with $\\alpha = %s$, %d outlier%s detected on difference values.",
              ftrim(alpha), length(d.o.result$o.i), ifelse(length(d.o.result$o.i) > 1, "s were", " was")
            )
            d <- d[-d.o.result$o.i]
            d.x <- d.x[-d.o.result$o.i]
            d.y <- d.y[-d.o.result$o.i]
          }


          opdt <- self$results$o$items$opd
          pd.o.result <- esd(pd, alpha)
          for (i in seq_along(pd.o.result$d.v)) {
            opdt$addRow(
              rowKey = i,
              values = list(
                x = pd.x[[pd.o.result$o.i[[i]]]],
                y = pd.y[[pd.o.result$o.i[[i]]]],
                b = pd.o.result$d.v[[i]],
                mean = pd.o.result$m[[i]],
                esd = pd.o.result$esd[[i]],
                sd = pd.o.result$sd[[i]],
                lambda = pd.o.result$lambda[[i]]
              )
            )
          }
          if (length(pd.o.result$o.i) > 0) {
            opdt$setVisible(TRUE)
            oc.r[[2]] <- tprintf(
              "According to the extreme studentized deviate test with $\\alpha = %s$, %d outlier%s detected on percent difference values.",
              ftrim(alpha), length(pd.o.result$o.i), ifelse(length(pd.o.result$o.i) > 1, "s were", " was")
            )
            pd <- pd[-pd.o.result$o.i]
            pd.x <- pd.x[-pd.o.result$o.i]
            pd.y <- pd.y[-pd.o.result$o.i]
          }
        }
        oc$setContent(oc.r)

        d.m <- mean(d)
        d.mid <- median(d)
        N <- length(d)
        d.se <- sd(d) / N
        pd.se <- sd(pd) / N
        tv <- qt(1 - alpha / 2, N - 1)
        pd.m <- mean(pd)
        pd.mid <- median(pd)
        ddf <- data.frame(x = (d.x + d.y) / 2, y = d)
        pddf <- data.frame(x = (pd.x + pd.y) / 2, y = pd)
        df$setState(list(pd = ddf))
        dhf$setState(list(d = d))
        pdf$setState(list(pd = pddf))
        pdhf$setState(list(d = pd))
        d.cimid <- median_ci(d, alpha)$conf.int
        pd.cimid <- median_ci(pd, alpha)$conf.int
        d.wci <- wilcoxon_ci(d, alpha)
        pd.wci <- wilcoxon_ci(pd, alpha)
        d.lm <- lm(y ~ x, ddf)
        d.leq <- tprintf(
          "$Y = %sx %s %s$",
          sp(coef(d.lm)[2], with.tex = FALSE),
          ifelse(coef(d.lm)[1] > 0, "+", "-"),
          sp(abs(coef(d.lm)[1]), with.tex = FALSE)
        )
        pd.lm <- lm(y ~ x, pddf)
        pd.leq <- tprintf(
          "$Y = %sx %s %s$",
          sp(coef(pd.lm)[2], with.tex = FALSE),
          ifelse(coef(pd.lm)[1] > 0, "+", "-"),
          sp(abs(coef(pd.lm)[1]), with.tex = FALSE)
        )
        d.pv <- summary(d.lm)$coefficients["x", "Pr(>|t|)"]
        pd.pv <- summary(pd.lm)$coefficients["x", "Pr(>|t|)"]
        t$addRow(
          rowKey = "d",
          values = list(
            t = "Difference",
            mean = d.m,
            mid = d.mid,
            cil = d.m - d.se * tv,
            ciu = d.m + d.se * tv,
            cimidl = d.cimid[[1]],
            cimidu = d.cimid[[2]],
            tukey = d.wci$mid,
            citukeyl = d.wci$lower,
            citukeyu = d.wci$upper
          )
        )
        t$addRow(
          rowKey = "p",
          values = list(
            t = "Difference (%)",
            mean = pd.m,
            mid = pd.mid,
            cil = pd.m - pd.se * tv,
            ciu = pd.m + pd.se * tv,
            cimidl = pd.cimid[[1]],
            cimidu = pd.cimid[[2]],
            tukey = pd.wci$mid,
            citukeyl = pd.wci$lower,
            citukeyu = pd.wci$upper
          )
        )
        d.nt <- shapiro.test(d)
        pd.nt <- shapiro.test(pd)

        dpc$setContent(
          c(
            # tprintf(
            #   "The OLS regression performed on difference plot %s with a $P = %s %s %s$ of slope means slope is%s significant and difference is %sconstant.",
            #   d.leq, ftrim(d.pv), ifelse(d.pv < alpha, "\\lt", "\\ge"), ftrim(alpha), ifelse(d.pv < alpha, "", " not"), ifelse(d.pv < alpha, "non", "")
            # ),
            # tprintf(
            #   "The OLS regression performed on percent difference plot %s with a $P = %s %s %s$ of slope means slope is%s significant and difference is %sconstant.",
            #   pd.leq, ftrim(pd.pv), ifelse(pd.pv < alpha, "\\lt", "\\ge"), ftrim(alpha), ifelse(pd.pv < alpha, "", " not"), ifelse(pd.pv < alpha, "non", "")
            # )
            tprintf(
              "The OLS regression on the difference plot %s yields a $P = %s %s %s$, indicating that the slope is%s statistically significant and the difference is %sconstant.",
              d.leq, ftrim(d.pv), ifelse(d.pv < alpha, "\\lt", "\\ge"), ftrim(alpha), ifelse(d.pv < alpha, "", " not"), ifelse(d.pv < alpha, "non", "")
            ),
            tprintf(
              "The OLS regression on the percent difference plot %s yields a $P = %s %s %s$, indicating that the slope is%s statistically significant and the difference is %sconstant.",
              pd.leq, ftrim(pd.pv), ifelse(pd.pv < alpha, "\\lt", "\\ge"), ftrim(alpha), ifelse(pd.pv < alpha, "", " not"), ifelse(pd.pv < alpha, "non", "")
            )
          )
        )
        ddpc$setContent(
          c(
            tprintf(
              "According to the Shapiro-Wilk test on difference, $P = %s %s %s$ indicating that the distribution%s the assumption of normality.",
              ftrim(d.nt$p.value), ifelse(d.nt$p.value < alpha, "\\lt", "\\ge"), ftrim(alpha), ifelse(d.nt$p.value < alpha, " does not satisfy", " satisfies")
            ),
            tprintf(
              "According to the Shapiro-Wilk test on percent difference, $P = %s %s %s$ indicating that the distribution%s the assumption of normality.",
              ftrim(pd.nt$p.value), ifelse(pd.nt$p.value < alpha, "\\lt", "\\ge"), ftrim(alpha), ifelse(pd.nt$p.value < alpha, " does not satisfy", " satisfies")
            )
          )
        )
        c$setContent(
          c(
            tprintf("The average difference for the entire measured interval ($\\overline{d}$):\n$\\overline{d} = \\sum_{i=1}^N{d_i}/N = %s$", ftrim(d.m)),
            tprintf("The median difference for the entire measured interval ($\\tilde{d}$):\n$\\tilde{d} = median(d_i) = %s$", ftrim(d.mid)),
            tprintf("The Hodges-Lehmann estimator of difference for the entire measured interval ($\\hat{d}$):\n$\\hat{d} = median((d_i+d_j)/2, i \\le j = 1,\\dots,N) = %s$", ftrim(d.wci$mid)),
            tprintf("The average percent difference for the entire measured interval ($\\overline{\\%%d}$):\n$\\overline{\\%%d} = 100\\%%\\cdot\\sum_{i=1}^N[2{d_i}/(x_i+y_i)]/N = %s\\%%$", ftrim(pd.m)),
            tprintf("The median percent difference for the entire measured interval ($\\widetilde{\\%%d}$):\n$\\widetilde{\\%%d} = 100\\%%\\cdot median(2d_i/(x_i+y_i)) = %s\\%%$", ftrim(pd.mid)),
            tprintf("The Hodges-Lehmann estimator of percent difference for the entire measured interval ($\\widehat{\\%%d}$):\n$\\widehat{\\%%d} = 100\\%%\\cdot median(d_i/(x_i+y_i)+d_j/(x_j+y_j), i \\le j = 1,\\dots,N) =  %s\\%%$", ftrim(pd.wci$mid))
          )
        )
      },
      .d.render = function(image, ...) {
        if (is.null(image$state)) {
          return(FALSE)
        }
        pd <- image$state$pd
        range.x <- range(pd$x)
        range.y <- range(pd$y)
        plot <- ggplot(data = pd, aes(x = x, y = y)) +
          geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
          geom_smooth(
            method = "lm", se = FALSE, color = "red",
            linetype = "solid", fullrange = TRUE, size = 0.5
          ) +
          geom_point(color = "#4F81BD", shape = 18, size = 5) +
          annotate("text",
            x = max(range.x), y = 0, label = "0.00",
            hjust = -0.5, vjust = -0.4, size = 5,
            family = "Times New Roman"
          ) +
          labs(
            x = "Concentration",
            y = "Difference"
          ) +
          scale_x_continuous(
            breaks = custom_breaks(range.x),
            limits = custom_breaks(range.x, 2),
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          scale_y_continuous(
            breaks = custom_breaks(range.y),
            limits = custom_breaks(range.y, 2),
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          theme_minimal() +
          theme(
            plot.margin = unit(c(0.5, 0.5, 0.2, 0.7), "cm"),
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
            axis.title.x = element_text(
              size = 16, family = "Times New Roman", margin = margin(t = 10)
            ),
            axis.title.y = element_text(
              size = 16, family = "Times New Roman", margin = margin(r = 5)
            ),
            axis.text.x = element_text(size = 14, family = "Times New Roman"),
            axis.text.y = element_text(size = 14, family = "Times New Roman")
          )
        print(plot)
        return(TRUE)
      },
      .dh.render = function(image, ...) {
        if (is.null(image$state)) {
          return(FALSE)
        }
        d <- image$state$d
        range.d <- range(d)
        r <- range.d[[2]] - range.d[[1]]
        plot <- ggplot(data = data.frame(x = d), aes(x = x)) +
          geom_histogram(aes(y = ..count..),
            binwidth = r / 10, fill = "#4F81BD",
            color = "black", alpha = 0.7
          ) +
          stat_function(
            fun = function(x) {
              dnorm(x,
                mean = mean(d),
                sd = sd(d)
              ) * length(d) * (r / 10)
            },
            color = "red", size = 0.5
          ) +
          labs(
            x = "Difference",
            y = "Count"
          ) +
          scale_x_continuous(
            breaks = custom_breaks(range.d),
            limits = custom_breaks(range.d, 2),
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          # scale_y_continuous(
          #   breaks = custom_breaks(range.y),
          #   limits = custom_breaks(range.y, 2),
          #   labels = label_number(accuracy = 0.01),
          #   expand = c(0, 0)
          # ) +
          theme_minimal() +
          theme(
            plot.margin = unit(c(0.5, 0.5, 0.2, 0.7), "cm"),
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
            axis.title.x = element_text(
              size = 16, family = "Times New Roman", margin = margin(t = 10)
            ),
            axis.title.y = element_text(
              size = 16, family = "Times New Roman", margin = margin(r = 5)
            ),
            axis.text.x = element_text(size = 14, family = "Times New Roman"),
            axis.text.y = element_text(size = 14, family = "Times New Roman")
          )
        print(plot)
        return(TRUE)
      }
    )
  )
}
