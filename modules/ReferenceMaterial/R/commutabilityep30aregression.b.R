# This file is a generated template, your changes will not be overwritten
library(ggplot2)
library(MethComp)
library(goftest)
library(lmtest)

CommutabilityEP30ARegressionClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "CommutabilityEP30ARegressionClass",
    inherit = CommutabilityEP30ARegressionBase,
    private = list(
      .run = function() {
        self$results$abb$setContent(self$results$abb$content)
        self$results$symbols$setContent(self$results$symbols$content)
        self$results$c$items$deming$items$h$setContent(
          self$results$c$items$deming$items$h$content
        )
        self$results$c$items$ols$items$h$setContent(
          self$results$c$items$ols$items$h$content
        )
        self$results$c$items$pb$items$h$setContent(
          self$results$c$items$pb$items$h$content
        )
        self$results$mr$setContent(
          c(
            "Characterization and Qualification of Commutable Reference Materials for Laboratory Medicine",
            "CLSI, EP30-2nd Edition",
            "August 2024"
          )
        )
        ols <- function(pd, odc, lil, uil) {
          x.m <- unlist(pd$cs.x.m)
          y.m <- unlist(pd$cs.y.m)
          df <- data.frame(x = x.m, y = y.m)
          model <- lm(y ~ x, df)
          ah <- coef(model)[[1]]
          bh <- coef(model)[[2]]
          rm.bh <- setNames(sapply(pd$rm.g.u, function(x) {
            return(-1 / bh)
          }), pd$rm.g.u)
          rm.ah <- setNames(sapply(pd$rm.g.u, function(rmid) {
            return(
              pd$rm.y.g.m[[rmid]] - rm.bh[[rmid]] * pd$rm.x.g.m[[rmid]]
            )
          }), pd$rm.g.u)
          rm.xf <- setNames(sapply(pd$rm.g.u, function(rmid) {
            return(
              (rm.ah[[rmid]] - ah) / (bh - rm.bh[[rmid]])
            )
          }), pd$rm.g.u)
          rm.yf <- setNames(sapply(pd$rm.g.u, function(rmid) {
            return(
              ah + bh * rm.xf[[rmid]]
            )
          }), pd$rm.g.u)
          rm.fd <- setNames(sapply(pd$rm.g.u, function(rmid) {
            return(
              sqrt((pd$rm.x.g.m[[rmid]] - rm.xf[[rmid]])^2 +
                (pd$rm.y.g.m[[rmid]] - rm.yf[[rmid]])^2)
            )
          }), pd$rm.g.u)
          rm.c <- setNames(sapply(pd$rm.g.u, function(rmid) {
            if (rm.fd[[rmid]] < lil) {
              return("Commutable")
            }
            if (rm.fd[[rmid]] < odc) {
              return("Indeterminate<br>(mean within criterion)")
            }
            if (rm.fd[[rmid]] < uil) {
              return("Indeterminate<br>(mean beyond criterion)")
            }
            return("Non-commutable")
          }), pd$rm.g.u)
          pi.x <- seq(min(x.m), max(x.m), length.out = 2)
          delta <- odc * 2 / sqrt(1 + bh^2)
          pi.b <- t(sapply(pi.x, function(x) {
            return(c(ah + bh * x - delta, ah + bh * x + delta))
          }))
          pi.rm <- lapply(pd$rm.x.g.m, function(x) {
            predict(model,
              newdata = data.frame(x = x),
              interval = "confidence", level = 1 - pd$alpha
            )[, c("lwr", "upr")]
          })
          return(list(
            ah = ah,
            bh = bh,
            x.m = x.m,
            y.m = y.m,
            pi.x = pi.x,
            pi.b = pi.b,
            rm.bh = rm.bh,
            rm.ah = rm.ah,
            rm.xf = rm.xf,
            rm.yf = rm.yf,
            rm.fd = rm.fd,
            rm.c = rm.c
          ))
        }

        pb <- function(pd, odc, lil, uil) {
          x.m <- unlist(pd$cs.x.m)
          y.m <- unlist(pd$cs.y.m)
          x.sd <- unlist(pd$cs.x.sd)
          y.sd <- unlist(pd$cs.y.sd)
          x.e.s2 <- mean(x.sd^2)
          y.e.s2 <- mean(y.sd^2)
          lambda <- y.e.s2 / x.e.s2
          model <- PBreg(
            x.m, y.m,
            conf.level = pd$alpha
          )
          coef.pbr <- coef(model)
          ah <- coef.pbr[1, 1]
          bh <- coef.pbr[2, 1]
          rm.bh <- setNames(sapply(pd$rm.g.u, function(x) {
            return(-1 / bh)
          }), pd$rm.g.u)
          rm.ah <- setNames(sapply(pd$rm.g.u, function(rmid) {
            return(
              pd$rm.y.g.m[[rmid]] - rm.bh[[rmid]] * pd$rm.x.g.m[[rmid]]
            )
          }), pd$rm.g.u)
          rm.xf <- setNames(sapply(pd$rm.g.u, function(rmid) {
            return(
              (rm.ah[[rmid]] - ah) / (bh - rm.bh[[rmid]])
            )
          }), pd$rm.g.u)
          rm.yf <- setNames(sapply(pd$rm.g.u, function(rmid) {
            return(
              ah + bh * rm.xf[[rmid]]
            )
          }), pd$rm.g.u)
          rm.fd <- setNames(sapply(pd$rm.g.u, function(rmid) {
            return(
              sqrt((pd$rm.x.g.m[[rmid]] - rm.xf[[rmid]])^2 +
                (pd$rm.y.g.m[[rmid]] - rm.yf[[rmid]])^2)
            )
          }), pd$rm.g.u)
          rm.c <- setNames(sapply(pd$rm.g.u, function(rmid) {
            if (rm.fd[[rmid]] < lil) {
              return("Commutable")
            }
            if (rm.fd[[rmid]] < odc) {
              return("Indeterminate<br>(mean within criterion)")
            }
            if (rm.fd[[rmid]] < uil) {
              return("Indeterminate<br>(mean beyond criterion)")
            }
            return("Non-commutable")
          }), pd$rm.g.u)
          pi.x <- seq(min(x.m), max(x.m), length.out = 2)
          delta <- odc * 2 / sqrt(1 + bh^2)
          pi.b <- t(sapply(pi.x, function(x) {
            return(c(ah + bh * x - delta, ah + bh * x + delta))
          }))
          return(list(
            ah = ah,
            bh = bh,
            x.m = x.m,
            y.m = y.m,
            pi.x = pi.x,
            pi.b = pi.b,
            x.e.s2 = x.e.s2,
            y.e.s2 = y.e.s2,
            mp = coef.pbr,
            lambda = lambda,
            rm.bh = rm.bh,
            rm.ah = rm.ah,
            rm.xf = rm.xf,
            rm.yf = rm.yf,
            rm.fd = rm.fd,
            rm.c = rm.c,
            pi.x = pi.x,
            pi.b = pi.b
          ))
        }
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        pd <- regression.prepare.data(self$data, self$options)
        if (self$options$ol) {
          self$results$descriptive$olg$setVisible(TRUE)
          pd <- ep14outliers(pd)
          if (pd$n.outliers > 0) {
            self$results$descriptive$olg$t$setVisible(TRUE)
            for (csid in names(pd$cs.id.outliers)) {
              if (pd$cs.id.outliers[[csid]]) {
                self$results$descriptive$olg$t$addRow(
                  rowKey = csid,
                  values = list(
                    csid = csid,
                    q = pd$qcv,
                    xr = pd$cs.x.r[[csid]],
                    xe = pd$cs.x.e.sd,
                    xqsd = pd$cs.x.qsd,
                    yr = pd$cs.y.r[[csid]],
                    ye = pd$cs.y.e.sd,
                    yqsd = pd$cs.y.qsd
                  )
                )
              }
            }
            self$results$descriptive$olg$c$setContent(
              paste0(
                "According to the Studentized range test, ",
                sprintf(
                  "ID %s data %s.",
                  join(pd$cs.id.removed),
                  ifelse(length(pd$cs.id.removed) > 1,
                    "are outliers", "is an outlier"
                  )
                )
              )
            )
          } else {
            self$results$descriptive$olg$t$setVisible(FALSE)
            self$results$descriptive$olg$c$setContent(
              "Non-declared outliers."
            )
          }
        } else {
          self$results$descriptive$olg$setVisible(FALSE)
        }
        dtest <- self$results$descriptive$dtg$items$dtest
        dtest$columns$xr$setTitle(
          sprintf("Passed test<br>($\\alpha$=%s)", ftrim(pd$alpha))
        )
        dtest$columns$yr$setTitle(
          sprintf("Passed test<br>($\\alpha$=%s)", ftrim(pd$alpha))
        )
        xv <- as.vector(as.matrix(pd$cs.x))
        yv <- as.vector(as.matrix(pd$cs.y))
        d.test <- distribution.test(xv, yv, pd$alpha)
        dtest$addRow(
          rowKey = "Uniform",
          values = list(
            dtype = "Uniform",
            method = "Anderson-Darling",
            xp = d.test$xu$p.value,
            xr = ifelse(d.test$xu$p.value < pd$alpha, "No", "Yes"),
            yp = d.test$yu$p.value,
            yr = ifelse(d.test$yu$p.value < pd$alpha, "No", "Yes")
          )
        )
        dtest$addRow(
          rowKey = "Normal",
          values = list(
            dtype = "Normal",
            method = "Anderson-Darling",
            xp = d.test$xn$p.value,
            xr = ifelse(d.test$xn$p.value < pd$alpha, "No", "Yes"),
            yp = d.test$yn$p.value,
            yr = ifelse(d.test$yn$p.value < pd$alpha, "No", "Yes")
          )
        )
        dtest$addRow(
          rowKey = "Exponential",
          values = list(
            dtype = "Exponential",
            method = "Anderson-Darling",
            xp = d.test$xe$p.value,
            xr = ifelse(d.test$xe$p.value < pd$alpha, "No", "Yes"),
            yp = d.test$ye$p.value,
            yr = ifelse(d.test$ye$p.value < pd$alpha, "No", "Yes")
          )
        )
        d.c <- d.conclusion(names(d.test$x.d.pass), names(d.test$y.d.pass))
        self$results$descriptive$dtg$c$setContent(d.c)


        uxm <- unlist(pd$cs.x.m)
        uym <- unlist(pd$cs.y.m)
        v.d <- visualization.data(uxm, uym)
        self$results$descriptive$items$v$or$setState(list(
          k = v.d$k,
          b = v.d$b,
          x = uxm,
          y = uym,
          unit = pd$unit
        ))
        self$results$descriptive$items$v$d$setState(list(
          y = uxm - uym,
          x = (uxm + uym) / 2,
          unit = pd$unit
        ))
        self$results$descriptive$items$v$c$setContent(
          v.d$conclusion
        )

        p.rm.se <- sqrt(
          (sum(pd$rm.x.sd^2) / length(pd$rm.g.u) +
            sum(pd$rm.y.sd^2) / length(pd$rm.g.u)
          ) / 2 / pd$k
        )
        cf <- 1.90
        ncc <- abs(self$options$ncc) * 3 / 8 * sqrt(3)
        hci <- p.rm.se * cf
        odc <- ncc
        dhci <- sqrt(hci^2 / 2)
        ocis <- sqrt(dhci^2 / 2)
        lil <- ncc - dhci
        uil <- ncc + dhci
        ccst <- self$results$ccs$items$t
        ccsc <- self$results$ccs$items$c
        ccst$addRow(
          rowKey = "ucrm",
          values = list(
            ssu = tprintf("Reference material(s)<br>($u\\text{max}_{rm}$)"),
            isua = tprintf("1/3 of $u\\text{max}_{cs}$"),
            csua = "",
            eccsu = ""
          )
        )
        ccst$addRow(
          rowKey = "ncrm",
          values = list(
            ssu = tprintf("Noncommutability of<br>reference material(s)<br>($u\\text{max}_{nc}$)"),
            isua = tprintf("3/8 of $u\\text{max}_{cs}$"),
            csua = tprintf("1/2 of $u\\text{max}_{cs}$"),
            eccsu = tprintf("$\\sqrt{u_{rm}^2+u_{nc}^2}$")
          )
        )
        ccst$addRow(
          rowKey = "cal",
          values = list(
            ssu = tprintf("Calibrator<br>($u\\text{max}_{cal}$)"),
            isua = tprintf("3/8 of $u\\text{max}_{cs}$"),
            csua = tprintf("5/8 of $u\\text{max}_{cs}$"),
            eccsu = tprintf("$\\sqrt{u_{rm}^2+u_{nc}^2+u_{cal}^2}$")
          )
        )
        ccst$addRow(
          rowKey = "m",
          values = list(
            ssu = tprintf("Measurement<br>($u\\text{max}_{rw}$)"),
            isua = tprintf("3/4 of $u\\text{max}_{cs}$"),
            csua = tprintf("$u\\text{max}_{cs}$"),
            eccsu = tprintf("$\\sqrt{u_{rm}^2+u_{nc}^2+u_{cal}^2+u_{rw}^2}$")
          )
        )
        ancc <- abs(self$options$ncc)
        ccsc$setContent(c(
          "The individual standard uncertainty allowance is following:",
          tprintf("$u\\text{max}_{rm}=1/3 \\text{ of } u\\text{max}_{cs}=%s$", ftrim(1 / 3 * ancc)),
          tprintf("$u\\text{max}_{nc}=3/8 \\text{ of } u\\text{max}_{cs}=%s$", ftrim(3 / 8 * ancc)),
          tprintf("$u\\text{max}_{cal}=3/8 \\text{ of } u\\text{max}_{cs}=%s$", ftrim(3 / 8 * ancc)),
          tprintf("$u\\text{max}_{rw}=3/4 \\text{ of } u\\text{max}_{cs}=%s$", ftrim(3 / 4 * ancc)),
          "The combined standard uncertainty allowance is following:",
          tprintf("$uc\\text{max}_{nc}=1/2 \\text{ of } u\\text{max}_{cs}=%s$", ftrim(1 / 2 * ancc)),
          tprintf("$uc\\text{max}_{cal}=3/8 \\text{ of } u\\text{max}_{cs}=%s$", ftrim(5 / 8 * ancc)),
          tprintf("$uc\\text{max}_{rw}=u\\text{max}_{cs}=%s$", ftrim(ancc)),
          tprintf("The maximum allowable noncommutability bias (Commutability criterion):\n$\\sqrt{3} u\\text{max}_{nc}=%.3f$", 3 * sqrt(3) / 8 * ancc)
          # tprintf("$%s,%s,%s,%s$", ftrim(1 / 3 * ancc), ftrim(3 / 8 * ancc), ftrim(3 / 8 * ancc), ftrim(3 / 4 * ancc))
          # tprintf("1. $u\\text{max}_{cs}=%s$", ftrim(ancc)),
          # tprintf("2. Reference material(s): $u\\text{max}_{nc}=%s$", ftrim(1 / 3 * ancc))
        ))
        self$results$oci$setContent(c(
          sprintf("Number of reference material sample replicates: %d", pd$k),
          sprintf("Pooled reference material standard error: %.3f", p.rm.se),
          sprintf("Coverage factor: %.3f", cf),
          sprintf("Half confidence interval (CI): %.3f = %.3f $\\times$ %.3f", hci, p.rm.se, cf),
          sprintf("Orthogonal delta criterion: %.3f", odc),
          sprintf("Delta half confidence interval: %.3f = $\\sqrt\\{%.3f^2/2\\}$", dhci, hci),
          sprintf("Lower indeterminate limit: %.3f = %.3f - %.3f", lil, ncc, dhci),
          sprintf("Upper indeterminate limit: %.3f = %.3f + %.3f", uil, ncc, dhci),
          sprintf("Orthogonal CI step: %.3f = $\\sqrt\\{%.3f^2/2\\}$", ocis, dhci),
          # texfmt("Orthogonal line slope: $\\hat{\\beta}_{H\\perp}=-1/\\hat{\\beta}_H$"),
          # texfmt("Orthogonal line intercept: $\\hat{\\alpha}_{H\\perp}=Y-\\hat{\\beta}_{H\\perp} X$"),
          # texfmt("X (on fit): $X_{fit} = (\\hat{\\alpha}_{H\\perp} - \\hat{\\alpha}_H)/(\\hat{\\beta}_H - \\hat{\\beta}_{H\\perp})$"),
          # texfmt("Y (on fit): $Y_{fit} = \\hat{\\alpha}_H +\\hat{\\beta}_H X_{fit}$"),
          # texfmt("RM - CS fit delta: $\\Delta_{fit} = \\sqrt{(X-X_{fit})^2+(Y-Y_{fit})^2}$")
          texfmt("Orthogonal line slope: $-1/\\hat{\\beta}_H$"),
          texfmt("Orthogonal line intercept: $Y-X \\cdot$ Orthogonal line slope"),
          texfmt("X (on fit): $X_{fit}=($Orthogonal line intercept $- \\hat{\\alpha}_H)/(\\hat{\\beta}_H-$ Orthogonal line slope$)$"),
          texfmt("Y (on fit): $Y_{fit}=\\hat{\\alpha}_H +\\hat{\\beta}_H \\cdot X_{fit}$"),
          texfmt("RM - CS fit delta: $\\sqrt{(X-X_{fit})^2+(Y-Y_{fit})^2}$")
        ))
        d <- ep30deming(pd, odc, lil, uil)
        o <- ols(pd, odc, lil, uil)
        p <- pb(pd, odc, lil, uil)
        c.r <- self$results$c$items
        for (rmid in pd$rm.g.u) {
          x <- pd$rm.x.g.m[[rmid]]
          y <- pd$rm.y.g.m[[rmid]]
          c.r$deming$items$tpf$items$t$addRow(
            rowKey = rmid,
            values = list(
              id = rmid,
              x = x,
              y = y,
              slope = d$rm.bh[[rmid]],
              intercept = d$rm.ah[[rmid]],
              xf = d$rm.xf[[rmid]],
              yf = d$rm.yf[[rmid]],
              fd = d$rm.fd[[rmid]],
              c = d$rm.c[[rmid]]
            )
          )
          c.r$ols$items$tpf$items$t$addRow(
            rowKey = rmid,
            values = list(
              id = rmid,
              x = x,
              y = y,
              slope = o$rm.bh[[rmid]],
              intercept = o$rm.ah[[rmid]],
              xf = o$rm.xf[[rmid]],
              yf = o$rm.yf[[rmid]],
              fd = o$rm.fd[[rmid]],
              c = o$rm.c[[rmid]]
            )
          )
          c.r$pb$items$tpf$items$t$addRow(
            rowKey = rmid,
            values = list(
              id = rmid,
              x = x,
              y = y,
              slope = p$rm.bh[[rmid]],
              intercept = p$rm.ah[[rmid]],
              xf = p$rm.xf[[rmid]],
              yf = p$rm.yf[[rmid]],
              fd = p$rm.fd[[rmid]],
              c = p$rm.c[[rmid]]
            )
          )
        }
        self$results$c$items$deming$items$c$setContent(
          c.conclusion(d$rm.c)
        )
        self$results$c$items$ols$items$c$setContent(
          c.conclusion(o$rm.c)
        )
        self$results$c$items$pb$items$c$setContent(
          c.conclusion(p$rm.c)
        )
        d.rnt <- res.deming.normal(d$bh, d$ah, uxm, uym, pd$alpha)
        self$results$c$items$deming$items$rn$setContent(
          d.rnt$conclusion
        )
        o.rnt <- res.ols.normal(o$bh, o$ah, uxm, uym, pd$alpha)
        self$results$c$items$ols$items$rn$setContent(
          o.rnt$conclusion
        )
        rvh <- rvh.test(o$bh, o$ah, uxm, uym, pd$alpha)
        self$results$c$items$ols$items$rvh$setContent(
          rvh$conclusion
        )
        self$results$c$items$deming$items$fpf$items$im$setState(
          list(
            cs.x = uxm,
            cs.y = uym,
            ah = d$ah,
            bh = d$bh,
            pi.x = d$pi.x,
            pi.b = d$pi.b,
            dhci = dhci,
            rm.x = unlist(pd$rm.x.g.m),
            rm.y = unlist(pd$rm.y.g.m),
            rm.id = unlist(pd$rm.id.u),
            unit = pd$unit
          )
        )
        # self$results$text$setContent(d)
        self$results$c$items$ols$items$fpf$items$im$setState(
          list(
            cs.x = uxm,
            cs.y = uym,
            ah = o$ah,
            bh = o$bh,
            pi.x = d$pi.x,
            pi.b = d$pi.b,
            dhci = dhci,
            rm.x = unlist(pd$rm.x.g.m),
            rm.y = unlist(pd$rm.y.g.m),
            rm.id = unlist(pd$rm.id.u),
            unit = pd$unit
          )
        )
        self$results$c$items$pb$items$fpf$items$im$setState(
          list(
            cs.x = uxm,
            cs.y = uym,
            ah = p$ah,
            bh = p$bh,
            pi.x = d$pi.x,
            pi.b = d$pi.b,
            dhci = dhci,
            rm.x = unlist(pd$rm.x.g.m),
            rm.y = unlist(pd$rm.y.g.m),
            rm.id = unlist(pd$rm.id.u),
            unit = pd$unit
          )
        )
        self$results$summary$setContent(
          c.summary(
            d.rnt$p.value >= pd$alpha,
            o.rnt$p.value >= pd$alpha,
            rvh$p.value >= pd$alpha,
            FALSE
          )
        )
      },
      .d.render = function(image, ...) {
        return(d.render(image))
      },
      .deming.render = function(image, ...) {
        if (is.null(image$state)) {
          return(FALSE)
        }
        xlabel <- "Measurement procedure X"
        ylabel <- "Measurement procedure Y"
        unit <- image$state$unit
        if (unit != "") {
          xlabel <- paste0(xlabel, " (", unit, ")")
          ylabel <- paste0(ylabel, " (", unit, ")")
        }
        cs.df <- data.frame(x = image$state$cs.x, y = image$state$cs.y)
        start.x <- min(image$state$cs.x)
        end.x <- max(image$state$cs.x)
        ah <- image$state$ah
        bh <- image$state$bh
        start.y <- start.x * bh + ah
        end.y <- end.x * bh + ah
        eq_label <- sprintf(
          "Y == %.2f*italic(x)%s%.2f",
          bh,
          ifelse(ah >= 0, "+ ", "- "),
          abs(ah)
        )
        b <- data.frame(
          x = image$state$pi.x,
          lower = image$state$pi.b[, 1],
          upper = image$state$pi.b[, 2]
        )
        dhci <- image$state$dhci
        k <- -1 / bh
        rm <- data.frame(
          x = image$state$rm.x,
          y = image$state$rm.y,
          id = image$state$rm.id,
          exl = image$state$rm.x - 1 / sqrt(1 + k^2) * dhci,
          exr = image$state$rm.x + 1 / sqrt(1 + k^2) * dhci,
          eyu = image$state$rm.y - k / sqrt(1 + k^2) * dhci,
          eyb = image$state$rm.y + k / sqrt(1 + k^2) * dhci,
          hjust = 1.5 * sign(image$state$rm.y - ah - bh * image$state$rm.x),
          vjust = -0.7 * sign(image$state$rm.y - ah - bh * image$state$rm.x)
        )
        dr <- data.frame(
          x = c(start.x, end.x),
          y = c(start.y, end.y)
        )
        range <- c(min(b$lower, cs.df$x, cs.df$y), max(b$upper, cs.df$x, cs.df$y))
        plot <- ggplot(cs.df, aes(x = x, y = y)) +
          geom_point(aes(color = "Clinical samples"), shape = 18, size = 5) +
          geom_point(
            data = rm,
            aes(x = x, y = y, color = "Reference materials"),
            size = 4
          ) +
          annotate("text",
            x = min(range), y = mean(range), label = eq_label, parse = TRUE,
            hjust = -0.1, vjust = -2, size = 5, family = "Times New Roman"
          ) +
          scale_x_continuous(
            breaks = custom_breaks(range),
            limits = custom_breaks(range, 2),
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          scale_y_continuous(
            breaks = custom_breaks(range),
            limits = custom_breaks(range, 2),
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          # coord_fixed() +
          labs(
            # caption = "Figure. Regression Line of MP y versus x",
            x = xlabel,
            y = ylabel
          ) +
          geom_line(
            data = dr,
            aes(
              x = x,
              y = y,
              color = "Regression line"
            ),
            size = 0.35
          ) +
          geom_line(
            data = b,
            aes(x = x, y = lower, color = "PI lower and upper limits"),
            linetype = "dashed"
          ) +
          geom_line(
            data = b,
            aes(x = x, y = upper, color = "PI lower and upper limits"),
            linetype = "dashed"
          ) +
          geom_segment(
            data = rm,
            aes(
              x = exl, y = eyu, xend = exr, yend = eyb
            ), color = "black"
          ) +
          scale_color_manual(
            name = "", values = c(
              "Clinical samples" = "#4F81BD",
              "Regression line" = "black",
              "Reference materials" = "red",
              "PI lower and upper limits" = "red"
            )
          ) +
          geom_text_repel(
            data = rm, aes(
              x = x, y = y, label = as.character(id),
              hjust = hjust, vjust = vjust
            ),
            fontface = "bold",
            color = "#e00f77",
            segment.color = NA,
            # hjust = 1.5, vjust = -0.7,
            size = 5, family = "Times New Roman"
          ) +
          theme_minimal() +
          theme(
            plot.margin = unit(c(1, 1, 0.5, 0.5), "cm"),
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
      },
      .ols.render = function(image, ...) {
        return(ols.render(image))
      }
    )
  )
}
