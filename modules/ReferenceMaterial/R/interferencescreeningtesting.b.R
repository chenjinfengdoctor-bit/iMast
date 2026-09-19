# This file is a generated template, your changes will not be overwritten

InterferenceScreeningTestingClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "InterferenceScreeningTestingClass",
    inherit = InterferenceScreeningTestingBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        im <- self$options$im
        alpha <- self$options$alpha
        l <- self$data[, self$options$l]
        vg.l <- split(self$data, l)
        t.l <- self$options$tl
        c.l <- self$options$cl
        mu <- self$options$mu
        wsd <- abs(self$options$wsd)
        wcv <- abs(self$options$wcv)
        wpd <- abs(self$options$wpd)
        dat <- self$results$da$items$t
        ist <- self$results$is$items$t
        conclusion <- self$results$c
        if (wsd <= 0) {
          dat$columns$wsd$setVisible(FALSE)
        }
        if (wcv <= 0) {
          dat$columns$wcv$setVisible(FALSE)
        }
        dat$columns$wsd$setTitle(
          paste0(dat$columns$wsd$title, " ", ftrim(wsd))
        )
        dat$columns$wcv$setTitle(
          paste0(dat$columns$wcv$title, " $\\pm$", ftrim(wcv), "%")
        )
        # dat$setTitle(paste0(dat$title, " of ", im))
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
        ist$setTitle(tprintf(ist$title, im))
        yes <- list()
        no <- list()
        ind <- list()
        for (level in unique(l)) {
          vg.l.tc <- split(
            vg.l[[level]][, self$options$v],
            vg.l[[level]][, self$options$g]
          )
          t <- vg.l.tc[[t.l]]
          c <- vg.l.tc[[c.l]]

          t.m <- mean(as.matrix(t))
          c.m <- mean(as.matrix(c))

          t.sd <- sd(as.matrix(t))
          c.sd <- sd(as.matrix(c))

          t.cv <- t.sd / t.m * 100
          c.cv <- c.sd / c.m * 100
          temp <- qt(1 - alpha / 2, length(unlist(t)) - 1) *
            t.sd / sqrt(length(unlist(t)))

          d <- t.m - c.m
          dci <- c(d - temp, d + temp)
          pd <- 100 * d / c.m
          pdci <- 100 * (dci / c.m)
          dat$addRow(
            rowKey = paste0(level, t.l),
            values = list(
              l = level, g = t.l, m = t.m, sd = t.sd, cv = t.cv,
              wsd = ifelse(abs(t.sd) < wsd, "Yes", "No"),
              wcv = ifelse(abs(t.cv) < wcv, "Yes", "No")
            )
          )
          dat$addRow(
            rowKey = paste0(level, c.l),
            values = list(
              l = level, g = c.l, m = c.m, sd = c.sd, cv = c.cv,
              wsd = ifelse(abs(t.sd) < wsd, "Yes", "No"),
              wcv = ifelse(abs(t.cv) < wcv, "Yes", "No")
            )
          )
          ist$addRow(
            rowKey = level,
            values = list(
              l = level, d = d, pd = pd,
              dcil = dci[[1]], dciu = dci[[2]],
              pdcil = pdci[[1]], pdciu = pdci[[2]],
              wpd = ifelse(abs(pd) < wpd, ifelse(
                max(abs(pdci)) < wpd,
                "Yes", "Indeterminate"
              ), "No")
            )
          )
          if (abs(pd) < wpd) {
            yes[[level]] <- TRUE
          } else {
            no[[level]] <- TRUE
          }
        }
        cr <- c()
        if (length(yes) > 0) {
          cr <- c(
            cr,
            tprintf(
              "Potential interference ($d_{obs}$(%%) vs $\\pm$%s%%) was not identified in level(s) %s of the measured samples.",
              ftrim(wpd), join(names(yes))
            )
          )
        }

        if (length(ind) > 0) {
          cr <- c(
            cr,
            tprintf(
              "Potential interference ($d_{obs}$(%%) vs $\\pm$%s%%) cannot be determined in level(s) %s of the measured samples.",
              ftrim(wpd), join(names(ind))
            )
          )
        }
        if (length(no) > 0) {
          cr <- c(
            cr,
            tprintf(
              "Potential interference ($d_{obs}$(%%) vs $\\pm$%s%%) was identified in level(s) %s of the measured samples.",
              ftrim(wpd), join(names(no))
            )
          )
        }
        conclusion$setContent(cr)
      }
    )
  )
}
