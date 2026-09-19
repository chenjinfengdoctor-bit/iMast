# This file is a generated template, your changes will not be overwritten

CharacterizingInterferenceEffectsClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "CharacterizingInterferenceEffectsClass",
    inherit = CharacterizingInterferenceEffectsBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        l <- self$data[, self$options$l]
        v <- self$data[, self$options$v]
        v.g <- split(v, l)
        wsd <- abs(self$options$wsd)
        wcv <- abs(self$options$wcv)
        wpd <- abs(self$options$wpd)
        dat <- self$results$da$items$t
        dam <- self$results$da$items$m
        pt <- self$results$p2p$items$t
        pf <- self$results$p2p$items$f
        pi <- self$results$p2p$items$i
        fd1 <- self$results$rb$items$fd1
        fd2 <- self$results$rb$items$fd2
        ri <- self$results$rb$items$i
        rc <- self$results$rb$items$c
        ist <- self$results$is$items$t
        isc <- self$results$is$items$c

        ni <- list()
        yi <- list()
        alpha <- self$options$alpha
        d.sign <- c()
        m.prev <- NA
        m0 <- 0
        mu <- self$options$mu
        mm <- self$options$mm
        iu <- self$options$iu
        if (wsd == 0) {
          dat$columns$wsd$setVisible(FALSE)
        }
        dat$columns$wsd$setTitle(
          paste0(dat$columns$wsd$title, " ", ftrim(wsd))
        )
        dat$columns$wcv$setTitle(
          paste0(dat$columns$wcv$title, " $\\pm$", ftrim(wcv), "%")
        )
        pt$columns$itv$setTitle(
          paste0(pt$columns$itv$title, " (", iu, ")")
        )
        # if (mu != "") {
        #   ist$columns$d$setTitle(
        #     paste0(ist$columns$d$title, " (", mu, ")")
        #   )
        # }
        ist$columns$dciu$setSuperTitle(
          tprintf(
            "%s%% %s",
            ftrim(100 * (1 - alpha)),
            ist$columns$dciu$superTitle
          )
        )
        ist$columns$dcil$setSuperTitle(
          tprintf(
            "%s%% %s",
            ftrim(100 * (1 - alpha)),
            ist$columns$dcil$superTitle
          )
        )
        ist$columns$pdciu$setSuperTitle(
          tprintf(
            "%s%% %s",
            ftrim(100 * (1 - alpha)),
            ist$columns$pdciu$superTitle
          )
        )
        ist$columns$pdcil$setSuperTitle(
          tprintf(
            "%s%% %s",
            ftrim(100 * (1 - alpha)),
            ist$columns$pdcil$superTitle
          )
        )
        ist$columns$wpd$setTitle(
          paste0(ist$columns$wpd$title, " $\\pm$", ftrim(wpd), "%")
        )
        m0 <- NULL
        for (item in self$options$ic) {
          id <- item$l
          c <- item$c
          m <- mean(unlist(v.g[[id]]))
          if (is.null(m0)) {
            m0 <- m
          }
          sd <- sd(unlist(v.g[[id]]))
          cv <- sd / m * 100
          dat$addRow(
            rowKey = id,
            values = list(
              s = id,
              r = as.numeric(c),
              m = m,
              dm = ifelse(is.na(m - m.prev), 0, m - m.prev),
              sd = sd,
              cv = cv,
              wsd = ifelse(abs(sd) < wsd, "Yes", "No"),
              wcv = ifelse(abs(cv) < wcv, "Yes", "No")
            )
          )
          if (!is.na(m.prev)) {
            d.sign <- c(d.sign, m - m.prev)
          }
          m.prev <- m
        }
        line.data <- data.frame(x = c(), y = c())
        point.data <- data.frame(x = c(), y = c())
        fd.data <- data.frame(x = c(), y = c())
        fpd.data <- data.frame(x = c(), y = c())
        for (item in self$options$ic) {
          id <- item$l
          c <- item$c
          m <- mean(unlist(v.g[[id]]))
          line.data <- rbind(line.data, data.frame(x = c, y = m))
          point.data <- rbind(
            point.data,
            data.frame(
              x = rep(c, length(unlist(v.g[[id]]))),
              y = unlist(v.g[[id]])
            )
          )
          fd.data <- rbind(
            fd.data,
            data.frame(
              x = rep(c, length(unlist(v.g[[id]]))),
              y = unlist(v.g[[id]]) - m0
            )
          )
          fpd.data <- rbind(
            fpd.data,
            data.frame(
              x = rep(c, length(unlist(v.g[[id]]))),
              y = 100 * (unlist(v.g[[id]]) - m0) / m0
            )
          )
          if (m0 == m) {
            next
          }

          temp <- qt(1 - alpha / 2, length(unlist(v.g[[id]])) - 1) *
            sd(unlist(v.g[[id]])) /
            sqrt(length(unlist(v.g[[id]])))
          d <- m - m0
          dci <- c(d - temp, d + temp)
          pd <- 100 * d / m0
          pdci <- 100 * (dci / m0)


          ist$addRow(
            rowKey = id,
            values = list(
              s = id,
              d = d,
              dcil = dci[[1]],
              dciu = dci[[2]],
              pd = pd,
              pdcil = pdci[[1]],
              pdciu = pdci[[2]],
              wpd = ifelse(abs(pd) < wpd,
                ifelse(max(abs(pdci)) < wpd,
                  "Yes", "Indeterminate"
                ), "No"
              )
            )
          )
          if (max(abs(pdci)) < wpd) {
            yi[[as.character(id)]] <- TRUE
          } else {
            ni[[as.character(id)]] <- TRUE
          }
        }
        if (length(ni) == 0) {
          isc$setContent(
            tprintf(
              paste0(
                "According to Dose-response experiment ($d_{obs}$(%%) vs $\\pm$%s%%),",
                " the interference was not identified at the number %s of sample%s."
              ),
              ftrim(wpd), join(names(yi)), ifelse(length(yi) > 1, "s", "")
            )
          )
        } else {
          isc$setContent(
            tprintf(
              paste0(
                "According to Dose-response experiment ($d_{obs}$(%%) vs $\\pm$%s%%),",
                " the interference was identified at the number %s of sample%s."
              ),
              ftrim(wpd), join(names(ni)), ifelse(length(ni) > 1, "s", "")
            )
          )
        }
        if (all(d.sign >= 0) || all(d.sign <= 0)) {
          dam$setContent(
            "Since all delta mean symbols are the same, monotonicity is satisfied."
          )
        } else {
          dam$setContent(
            "Monotonicity is not satisfied, which can occur as an artifact of both the measurement procedure's inherent imprecision and the limited number of replicates processed in the study."
          )
        }
        pf$setState(list(line.data = line.data, point.data = point.data))
        fd1$setState(list(
          p = 1, point.data = point.data,
          xlabel = "The concentration of interference material", ylabel = ifelse(
            self$options$mu != "", paste0("Difference", " (", self$options$mm, ", ", self$options$mu, ")"), "Difference"
          )
        ))
        fd2$setState(list(
          p = 2, point.data = point.data,
          xlabel = "The concentration of interference material", ylabel = ifelse(
            self$options$mu != "", paste0("Difference", " (", self$options$mm, ", ", self$options$mu, ")"), "Difference"
          )
        ))
        l1 <- lm(y ~ x, point.data)
        l2 <- lm(y ~ x + I(x^2), point.data)

        if (self$options$cb || self$options$eb) {
          pi$setVisible(TRUE)
          ri$setVisible(TRUE)
        }
        pic <- c()
        ric <- c()
        for (i in 1:(length(self$options$ic) - 1)) {
          iteml <- self$options$ic[[i]]
          itemr <- self$options$ic[[i + 1]]
          idl <- iteml$l
          idr <- itemr$l
          cl <- iteml$c
          cr <- itemr$c
          ml <- mean(unlist(v.g[[idl]]))
          mr <- mean(unlist(v.g[[idr]]))
          slope <- (mr - ml) / (cr - cl)
          intercept <- ml - cl * slope
          if (self$options$cb) {
            if (self$options$c <= max(cr, cl) && self$options$c > min(cr, cl)) {
              pic <- c(
                pic,
                tprintf(
                  "Following the equation: $Y = %sx %s %s$, the difference was %s%s at an interference concentration of %s%s.",
                  # "For known interference concentration %s%s, the effect (relative difference) calculated with fitting equation $Y = %sx %s %s$ was %s%s.",
                  ftrim(slope), ifelse(intercept > 0, "+", ""), ftrim(intercept),
                  ftrim(slope * self$options$c + intercept - m0),
                  ifelse(self$options$mu == "", "", paste0(" ", self$options$mu)),
                  ftrim(self$options$c),
                  ifelse(self$options$iu == "", "", paste0(" ", self$options$iu))
                )
              )
            }
          }
          if (self$options$eb) {
            if (m0 + self$options$e <= max(mr, ml) && m0 + self$options$e > min(mr, ml)) {
              pic <- c(
                pic,
                tprintf(
                  "Following the equation: $Y = %sx %s %s$, the interference concentration was %s%s at an difference of %s%s.",
                  ftrim(slope), ifelse(intercept > 0, "+", ""), ftrim(intercept),
                  ftrim((self$options$e - intercept + m0) / slope),
                  ifelse(self$options$iu == "", "", paste0(" ", self$options$iu)),
                  ftrim(self$options$e),
                  ifelse(self$options$mu == "", "", paste0(" ", self$options$mu))
                )
              )
            }
          }

          pt$addRow(
            rowKey = i,
            values = list(
              itv = tprintf("%.2f-%.2f", as.numeric(cl), as.numeric(cr)), s = slope, i = intercept,
              eq = tprintf("$Y=%sx%s%s$", ftrim(slope), ifelse(intercept > 0, "+", "-"), ftrim(abs(intercept)))
            )
          )
        }
        pi$setContent(pic)
        r1i <- coef(l1)[[1]]
        r1s <- coef(l1)[[2]]
        r2s1 <- coef(l2)[[2]]
        r2s2 <- coef(l2)[[3]]
        r2i <- coef(l2)[[1]]
        leq <- tprintf(
          "$Y = %sx %s %s$",
          sp(r1s, with.tex = FALSE), ifelse(r1i > 0, "+", ""), sp(r1i, with.tex = FALSE)
        )
        qeq <- tprintf(
          "$Y = %sx^2 %s %sx %s %s$",
          sp(r2s2, with.tex = FALSE), ifelse(r2s1 > 0, "+", ""),
          sp(r2s1, with.tex = FALSE), ifelse(r2i > 0, "+", ""), sp(r2i, with.tex = FALSE)
        )
        rc$setContent(
          c(
            tprintf(
              "Fitting equation (primary polynomial): %s, with $R^2$ of $%s$.",
              leq, ftrim(summary(l1)$r.squared)
            ),
            tprintf(
              "Fitting equation (quadratic polynomial): %s, with $R^2$ of $%s$.",
              qeq, ftrim(summary(l2)$r.squared)
            )
          )
        )
        if (self$options$cb) {
          ric <- c(
            ric,
            tprintf(
              "Following the equation: %s, the difference was %s%s at an interference concentration of %s%s.",
              leq,
              ftrim(r1s * self$options$c + r1i - m0),
              ifelse(self$options$mu == "", "", paste0(" ", self$options$mu)),
              ftrim(self$options$c), ifelse(self$options$iu == "", "", paste0(" ", self$options$iu))
            ),
            tprintf(
              "Following the equation: %s, the difference was %s%s at an interference concentration of %s%s.",
              qeq,
              ftrim(r2s2 * self$options$c^2 + r2s1 * self$options$c + r2i - m0),
              ifelse(self$options$mu == "", "", paste0(" ", self$options$mu)),
              ftrim(self$options$c), ifelse(self$options$iu == "", "", paste0(" ", self$options$iu))
            )
          )
        }
        if (self$options$eb) {
          v <- (self$options$e + m0 - r1i) / r1s
          ric <- c(
            ric,
            tprintf(
              "Following the equation: %s, the interference concentration was %s%s at an difference of %s%s.",
              leq, ftrim(v),
              ifelse(self$options$iu == "", "", paste0(" ", self$options$iu)),
              ftrim(self$options$e),
              ifelse(self$options$mu == "", "", paste0(" ", self$options$mu))
            )
          )
          sol <- solve(r2s2, r2s1, r2i - self$options$e - m0, range.in = range(fd.data$x))
          if (!is.null(sol)) {
            ric <- c(
              ric,
              tprintf(
                "Following the equation: %s, the interference concentration was %s%s at an difference of %s%s.",
                qeq, join(sapply(sol, ftrim)),
                ifelse(self$options$iu == "", "", paste0(" ", self$options$iu)),
                ftrim(self$options$e),
                ifelse(self$options$mu == "", "", paste0(" ", self$options$mu))
              )
            )
          }
        }
        ri$setContent(ric)
      },
      .p2p.render = function(image, ...) {
        if (is.null(image$state)) {
          return(FALSE)
        }
        ld <- image$state$line.data
        pd <- image$state$point.data
        range.x <- c(max(0, min(ld$x)), max(ld$x))
        range.y <- c(max(0, min(pd$y)), max(pd$y))
        xlabel <- "The concentration of interference material"
        ylabel <- "Measurand"
        dlabel <- "Difference"
        if (self$options$iu != "") {
          xlabel <- paste0(xlabel, " (", self$options$im, ", ", self$options$iu, ")")
        }
        if (self$options$mu != "") {
          ylabel <- paste0(ylabel, " (", self$options$mm, ", ", self$options$mu, ")")
          dlabel <- paste0(dlabel, " (", self$options$mm, ", ", self$options$mu, ")")
        }
        min.y <- min(ld$y)
        plot <- ggplot() +
          geom_line(data = ld, aes(color = "Fitting line", x = x, y = y), size = 0.35) +
          geom_point(
            data = pd, aes(color = "Measurand", x = x, y = y),
            shape = 18, size = 5
          ) +
          scale_color_manual(
            name = "", values = c(
              "Measurand" = "#4F81BD",
              "Fitting line" = "black"
            )
          ) +
          labs(
            x = xlabel,
            y = ylabel
          ) +
          scale_x_continuous(
            breaks = ld$x,
            limits = custom_breaks(range.x, 2),
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          scale_y_continuous(
            breaks = custom_breaks(range.y),
            limits = custom_breaks(range.y, 2),
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0),
            sec.axis = sec_axis(~ . - min.y,
              name = dlabel,
              breaks = custom_breaks(range.y - min.y),
              # limits = custom_breaks(range.y - min.y, 2),
              labels = label_number(accuracy = 0.01)
            )
          ) +
          theme_minimal() +
          theme(
            plot.margin = unit(c(0.7, 0.7, 0.2, 0.7), "cm"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.line = element_line(color = "black"),
            # legend.position = "right",
            legend.position = c(0.5, 0.15),
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
      },
      .rb.render = function(image, ...) {
        if (is.null(image$state)) {
          return(FALSE)
        }
        fd <- image$state$point.data
        lm <- lm(y ~ x, data = fd)
        xlabel <- image$state$xlabel
        ylabel <- image$state$ylabel
        range.x <- c(min(fd$x), max(fd$x))
        range.y <- c(min(fd$y), max(fd$y))
        if (self$options$iu != "") {
          xlabel <- paste0(xlabel, " (", self$options$im, ", ", self$options$iu, ")")
        }
        scml <- list("Regression line" = "black")
        scml[[image$state$ylabel]] <- "#4F81BD"
        if (image$state$p == 2) {
          fml <- y~x + I(x^2)
        } else {
          fml <- y ~ x
        }
        plot <- ggplot(data = fd, aes(x = x, y = y)) +
          geom_point(aes(color = image$state$ylabel), shape = 18, size = 5) +
          geom_smooth(
            method = "lm",
            formula = fml,
            level = 1 - self$options$alpha,
            se = TRUE, aes(color = "Regression line"),
            size = 0.35
          ) +
          labs(
            x = xlabel,
            y = ylabel
          ) +
          scale_color_manual(
            name = "", values = unlist(scml)
          ) +
          scale_x_continuous(
            breaks = custom_breaks(range.x),
            limits = custom_breaks(range.x, 2),
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          scale_y_continuous(
            breaks = custom_breaks(range.y),
            limits = custom_breaks(c(min(fd$y), max(fd$y)), 2),
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          theme_minimal() +
          theme(
            plot.margin = unit(c(0.7, 0.7, 0.2, 0.7), "cm"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(color = "black", size = 0.5),
            axis.line = element_line(color = "black"),
            # legend.position = "right",
            legend.position = c(0.5, 0.15),
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
