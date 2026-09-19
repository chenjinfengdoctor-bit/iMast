# This file is a generated template, your changes will not be overwritten
library(ggplot2)
# library(MethComp)
library(mcr)
library(goftest)
library(lmtest)



CommutabilityEP14A3Class <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "CommutabilityEP14A3Class",
    inherit = CommutabilityEP14A3Base,
    private = list(
      .run = function() {
        ols <- function(pd) {
          x.m <- unlist(pd$cs.x.m)
          y.m <- unlist(pd$cs.y.m)
          df <- data.frame(x = x.m, y = y.m)
          model <- lm(y ~ x, df)
          ah <- coef(model)[[1]]
          bh <- coef(model)[[2]]
          pi.x <- seq(min(x.m), max(x.m), length.out = 20)
          x.m.m <- mean(x.m)
          n <- length(x.m)
          v <- n - 2
          ss.res <- deviance(model)
          res.sum2 <- sum((x.m - x.m.m)^2)
          s.y.x <- sqrt(ss.res / v)
          t <- qt(1 - pd$alpha / 2, v)
          pi <- function(x.m.pc) {
            y.pc.pred <- ah + bh * x.m.pc
            s.y0 <- s.y.x * sqrt(1 + 1 / n + (x.m.pc - x.m.m)^2 / res.sum2)
            return(c(
              y.pc.pred - t * s.y0,
              y.pc.pred + t * s.y0
            ))
          }
          pi.b <- t(sapply(pi.x, pi))
          pi.rm <- lapply(pd$rm.x.g.m, pi)
          # pi.b <- predict(
          #   model,
          #   newdata = data.frame(x = pi.x),
          #   interval = "prediction", level = 1 - pd$alpha
          # )[, c("lwr", "upr")]

          # pi.rm <- lapply(pd$rm.x.g.m, function(x) {
          #   predict(model,
          #     newdata = data.frame(x = x),
          #     interval = "prediction", level = 1 - pd$alpha
          #   )[, c("lwr", "upr")]
          # })
          rm.c <- setNames(sapply(pd$rm.g.u, function(rmid) {
            y <- pd$rm.y.g.m[[rmid]]
            return(ifelse(
              y <= pi.rm[[rmid]][[2]] && y >= pi.rm[[rmid]][[1]],
              "Commutable",
              "Non-commutable"
            ))
          }), pd$rm.g.u)
          return(list(
            ah = ah,
            bh = bh,
            x.m = x.m,
            y.m = y.m,
            pi.x = pi.x,
            pi.b = pi.b,
            pi.rm = pi.rm,
            rm.c = rm.c,
            x.m.m = x.m.m,
            n = n,
            v = v,
            ss.res = ss.res,
            res.sum2 = res.sum2,
            s.y.x = s.y.x,
            t = t
          ))
        }
        pb.bootstrap <- function(pd, pi.x, pi.x.rm, B = 5000, seed = 1) {
          set.seed(seed)
          alpha <- pd$alpha
          x.m <- unlist(pd$cs.x.m)
          y.m <- unlist(pd$cs.y.m)

          y.hat.mat <- matrix(NA, nrow = B, ncol = length(pi.x))
          n <- length(x.m)
          for (b in 1:B) {
            idx <- sample(seq_along(x.m), size = n, replace = TRUE)
            x.b <- x.m[idx]
            y.b <- y.m[idx]

            # fit <- PBreg(x.b, y.b, conf.level = alpha)
            fit <- mcreg(x.b, y.b, method.reg = "PaBa", method.ci = "analytical")
            coefs <- coef(fit)
            a.b <- coefs[1, 1]
            b.b <- coefs[2, 1]
            y.hat <- a.b + b.b * x.b
            x.hat <- (y.b - a.b) / b.b
            r.y <- y.b - y.hat
            r.x <- x.b - x.hat
            r.o <- (b.b * x.b - y.b + a.b) / sqrt(b.b^2 + 1)
            r.o.x <- r.o * b.b / sqrt(b.b^2 + 1)
            r.o.y <- -r.o / sqrt(b.b^2 + 1)
            noise_i <- sample(1:n, size = length(pi.x), replace = TRUE)
            y.hat.mat[b, ] <- a.b + b.b * (pi.x - r.o.x[noise_i]) + r.o.y[noise_i]
          }
          y.lower <- apply(y.hat.mat, 2, quantile, probs = alpha / 2)
          y.upper <- apply(y.hat.mat, 2, quantile, probs = 1 - alpha / 2)

          y.rm.lower <- approx(x = pi.x, y = y.lower, xout = unlist(pi.x.rm), rule = 2)$y
          y.rm.upper <- approx(x = pi.x, y = y.upper, xout = unlist(pi.x.rm), rule = 2)$y

          pi.rm.result <- mapply(function(lower, upper) {
            c(lower = lower, upper = upper)
          }, y.rm.lower, y.rm.upper, SIMPLIFY = FALSE)

          names(pi.rm.result) <- names(pd$rm.x.g.m)
          return(list(
            pi.x = pi.x,
            pi.b = cbind(y.lower, y.upper),
            pi.rm = pi.rm.result,
            B = B,
            seed = seed
          ))
        }
        pb <- function(pd) {
          x.m <- unlist(pd$cs.x.m)
          y.m <- unlist(pd$cs.y.m)
          x.sd <- unlist(pd$cs.x.sd)
          y.sd <- unlist(pd$cs.y.sd)
          x.e.s2 <- mean(x.sd^2)
          y.e.s2 <- mean(y.sd^2)
          lambda <- y.e.s2 / x.e.s2
          model <- mcreg(
            x.m, y.m,
            alpha = pd$alpha
          )
          coef.pbr <- coef(model)
          ah <- coef.pbr[1, 1]
          bh <- coef.pbr[2, 1]

          pi.x <- seq(min(x.m), max(x.m), length.out = 50)
          # ci_lower_intercept <- coef.pbr[1, 2]
          # ci_upper_intercept <- coef.pbr[1, 3]
          # ci_lower_slope <- coef.pbr[2, 2]
          # ci_upper_slope <- coef.pbr[2, 3]

          # pi.b <- t(sapply(pi.x, function(xi) {
          #   lower <- ci_lower_intercept + ci_lower_slope * xi
          #   upper <- ci_upper_intercept + ci_upper_slope * xi
          #   c(lower, upper)
          # }))
          # pi.rm <- lapply(pd$rm.x.g.m, function(xi) {
          #   lower <- ci_lower_intercept + ci_lower_slope * xi
          #   upper <- ci_upper_intercept + ci_upper_slope * xi
          #   c(lower, upper)
          # })
          pb.b <- pb.bootstrap(pd, pi.x, pd$rm.x.g.m)
          pi.b <- pb.b$pi.b
          pi.rm <- pb.b$pi.rm
          rm.c <- setNames(sapply(pd$rm.g.u, function(rmid) {
            y <- pd$rm.y.g.m[[rmid]]
            return(ifelse(
              y <= pi.rm[[rmid]][[2]] && y >= pi.rm[[rmid]][[1]],
              "Commutable",
              "Non-commutable"
            ))
          }), pd$rm.g.u)
          return(list(
            ah = ah,
            bh = bh,
            x.m = x.m,
            y.m = y.m,
            pi.x = pi.x,
            pi.b = pi.b,
            pi.rm = pi.rm,
            x.e.s2 = x.e.s2,
            y.e.s2 = y.e.s2,
            mp = coef.pbr,
            lambda = lambda,
            rm.c = rm.c,
            pb.b = pb.b
          ))
        }
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        self$results$mr$setContent(c(
          "Evaluation of Commutability of Processed Samples",
          "CLSI, EP14-4th Edition",
          "July 2022"
        ))

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
        d <- ep14deming(pd)
        o <- ols(pd)
        p <- pb(pd)
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
              yl = d$pi.rm[[rmid]][[1]],
              yu = d$pi.rm[[rmid]][[2]],
              c = d$rm.c[[rmid]]
            )
          )
          c.r$ols$items$tpf$items$t$addRow(
            rowKey = rmid,
            values = list(
              id = rmid,
              x = x,
              y = y,
              yl = o$pi.rm[[rmid]][[1]],
              yu = o$pi.rm[[rmid]][[2]],
              c = o$rm.c[[rmid]]
            )
          )
          c.r$pb$items$tpf$items$t$addRow(
            rowKey = rmid,
            values = list(
              id = rmid,
              x = x,
              y = y,
              yl = p$pi.rm[[rmid]][[1]],
              yu = p$pi.rm[[rmid]][[2]],
              c = p$rm.c[[rmid]]
            )
          )
        }
        c.r$deming$items$pi$setContent(
          c(
            texfmt("The means of each sample's X and Y and then grand means:"),
            sprintf(
              texfmt("$\\overline{\\overline X} =%.3f,\\overline{\\overline Y} =%.3f$"),
              d$x.m.m, d$y.m.m
            ),
            texfmt("The estimated population variances for X and Y and their cross product:"),
            sprintf(
              texfmt("$\\hat \\sigma^2_{\\overline X}=%.3f,\\hat \\sigma^2_{\\overline Y}= %.3f,\\text{ and }\\hat \\sigma_{\\overline X \\overline Y}  = %.3f$"),
              d$x.m.s2, d$y.m.s2, d$xy.m.s
            ),
            texfmt("The random error variances and $\\lambda$:"),
            sprintf(
              texfmt("$\\hat \\sigma^2(\\varepsilon_X) = %.3f,\\hat \\sigma^2(\\varepsilon_Y) = %.3f, \\text{ and }\\hat \\lambda =%.3f$"),
              d$x.e.s2, d$y.e.s2, d$lambda
            ),
            "The slope and the intercept:",
            sprintf(texfmt("$\\hat \\beta_H =%.3f,\\hat \\alpha_H =%.3f$"), d$bh, d$ah),
            texfmt("The predict interval limits with $\\overline X_{Pc}$ as the value of X:"),
            sprintf(texfmt("$\\hat \\sigma_{\\beta H}^2 =%f$"), d$bh.e.s2),
            texfmt("$\\sigma(\\overline Y_{Pc\\_pred}) \\approx \\sqrt{(\\overline X_{Pc}-\\overline{\\overline X}^2)\\hat \\sigma^2_{\\beta H}+(\\hat \\beta^2_H\\hat \\sigma^2(\\varepsilon_X)+\\hat \\sigma^2(\\varepsilon_Y))(1+1/n)/N_{Pc}}$"),
            texfmt("$[Y_{Lower},Y_{Upper}]=\\overline Y_{Pc\\_pred}\\mp t[1-\\gamma/2,n(N_H-1)]\\cdot\\hat \\sigma(\\overline Y_{Pc\\_pred})$"),
            sprintf(texfmt("Where the two-tailed $t$ value for $p=%.3f$ and $n(N_H-1)=%d$ degrees of freedom is %.3f."), pd$alpha, d$n * (d$nh - 1), d$t)
          )
        )
        c.r$ols$items$pi$setContent(
          c(
            "The standard deviation of residuals:",
            tprintf(
              "$v_{residual} = n - 2 = %d - 2 = %d,\\text{ and } SS_{residual} = %.3f$",
              o$n, o$v, o$ss.res
            ),
            tprintf("$S_{Y \\cdot X} = \\sqrt{SS_{residual}/v} = %.3f$", o$s.y.x),
            "The slope and the intercept:",
            tprintf("$\\hat \\beta_H =%.3f,\\hat \\alpha_H =%.3f$", o$bh, o$ah),
            tprintf("For a given $\\overline X_{Pc}$, $\\overline Y_{Pc\\_pred}$ got a standard devitaion $\\hat\\sigma(\\overline Y_{Pc\\_pred})$:"),
            tprintf("$\\overline{\\overline X} = %.3f, \\sum(\\overline X-\\overline{\\overline X})^2 = %.3f$", o$x.m.m, o$res.sum2),
            tprintf("$\\hat\\sigma(\\overline Y_{Pc\\_pred}) = S_{Y\\cdot X}\\sqrt{1+1/n+(\\overline X_{Pc}-\\overline{\\overline X})^2/\\sum(\\overline X - \\overline{\\overline X})^2}$"),
            tprintf("$Y_{Pc\\_pred} =\\hat \\beta_H \\overline X_{Pc}+ \\hat \\alpha_H$"),
            tprintf("$[Y_{Lower},Y_{Upper}] = \\overline Y_{Pc\\_pred} \\mp t[1-\\gamma/2,v_{residual}]\\hat\\sigma(\\overline Y_{Pc\\_pred})$"),
            tprintf("Where the two-tailed $t$ value for $p=%.3f$ and $v_{residual}=%d$ is $%.3f$.", pd$alpha, o$v, o$t)
          )
        )
        c.r$pb$items$pi$setContent(
          c(
            "The slope and the intercept:",
            tprintf("$\\hat \\beta_H =%.3f,\\hat \\alpha_H =%.3f$", p$bh, p$ah),
            "The modified bootstrap method was used to determine the prediction interval including the upper and lower limits, the details are as follows:",
            "(1) Resampling:",
            tprintf("Sampling was conducted %d times on the clinical samples.", p$pb.b$B),
            "(2) Regression line fitting:",
            tprintf("Passing-Bablok regression line was fitted using the sampled data each time, and a total of %d regression lines were fitted.", p$pb.b$B),
            "(3) Random error estimating:",
            tprintf(paste0(
              "For each fitting result, ",
              "calculate the orthogonal residual $o_r$ between the regression line and the resampled scatter point $(X,Y)$, ",
              "using the formula: $o_r=(\\hat{\\beta}_H X-Y+\\hat{\\alpha}_H)/\\sqrt{\\hat{\\beta}_H^2+1}$."
            )),
            "(4) Statistical Model Design: ",
            tprintf(paste0(
              "Within the range of $X$ values measured in the clinical samples, ",
              "50 equally spaced points were selected and denoted as $X'$. ",
              "For each regression line, $Y'$ was defined as: $Y' = \\hat{\\beta}_H(X'-X_r) + \\hat{\\alpha}_H + Y_r$, ",
              "where $X_r,Y_r$ were calculated from the 50 orthogonal residuals $o_r$ obtained through resampling by: ",
              "$X_r=\\hat{\\beta}_H o_r/\\sqrt{\\hat{\\beta}_H^2+1},Y_r=-o_r/\\sqrt{\\hat{\\beta}_H^2+1}$."
            )),
            "(5) Prediction interval estimating:",
            tprintf(
              "Prediction intervals were determined based on the %s$^{th}$ and the %s$^{th}$ percentiles of all $Y'$ values.",
               ftrim(100 * pd$alpha / 2), ftrim(100 - 100 * pd$alpha / 2)),
            "(6) Simulation $Y$ value intervals of reference materials",
            "Simulation $Y$ values of reference material values from measurement procedure $X$ were determined based on above estimated prediction intervals."
          )
        )
        c.r$deming$items$c$setContent(
          c.conclusion(d$rm.c)
        )
        c.r$ols$items$c$setContent(
          c.conclusion(o$rm.c)
        )
        c.r$pb$items$c$setContent(
          c.conclusion(p$rm.c)
        )

        d.rnt <- res.deming.normal(d$bh, d$ah, uxm, uym, pd$alpha)
        c.r$deming$items$rn$setContent(
          d.rnt$conclusion
        )
        o.rnt <- res.ols.normal(o$bh, o$ah, uxm, uym, pd$alpha)
        c.r$ols$items$rn$setContent(
          o.rnt$conclusion
        )
        rvh <- rvh.test(o$bh, o$ah, uxm, uym, pd$alpha)
        c.r$ols$items$rvh$setContent(
          rvh$conclusion
        )
        c.r$deming$items$fpf$items$im$setState(
          list(
            cs.x = uxm,
            cs.y = uym,
            ah = d$ah,
            bh = d$bh,
            pi.x = d$pi.x,
            pi.b = d$pi.b,
            rm.x = unlist(pd$rm.x.g.m),
            rm.y = unlist(pd$rm.y.g.m),
            rm.id = unlist(pd$rm.id.u),
            unit = pd$unit
          )
        )
        c.r$ols$items$fpf$items$im$setState(
          list(
            cs.x = uxm,
            cs.y = uym,
            ah = o$ah,
            bh = o$bh,
            pi.x = o$pi.x,
            pi.b = o$pi.b,
            rm.x = unlist(pd$rm.x.g.m),
            rm.y = unlist(pd$rm.y.g.m),
            rm.id = unlist(pd$rm.id.u),
            unit = pd$unit
          )
        )
        c.r$pb$items$fpf$items$im$setState(
          list(
            cs.x = uxm,
            cs.y = uym,
            ah = p$ah,
            bh = p$bh,
            pi.x = p$pi.x,
            pi.b = p$pi.b,
            rm.x = unlist(pd$rm.x.g.m),
            rm.y = unlist(pd$rm.y.g.m),
            rm.id = unlist(pd$rm.id.u),
            unit = pd$unit
          )
        )
        d.pi.i.m <- mean(sapply(d$pi.rm, function(item) {
          item[2] - item[1]
        }))
        p.pi.i.m <- mean(sapply(p$pi.rm, function(item) {
          item[2] - item[1]
        }))
        self$results$summary$setContent(
          c.summary(
            d.rnt$p.value >= pd$alpha,
            o.rnt$p.value >= pd$alpha,
            rvh$p.value >= pd$alpha,
            d.pi.i.m < 1 / 3 * p.pi.i.m
          )
        )
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

        eq_label <- sprintf(
          "Y == %.2f * italic(x)%s%.2f",
          bh,
          ifelse(ah >= 0, "+ ", "- "),
          abs(ah)
        )
        start.y <- start.x * bh + ah
        end.y <- end.x * bh + ah
        b <- data.frame(
          x = image$state$pi.x,
          lower = image$state$pi.b[, 1],
          upper = image$state$pi.b[, 2]
        )
        rm <- data.frame(
          x = image$state$rm.x,
          y = image$state$rm.y,
          id = image$state$rm.id,
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
            hjust = -0.1, vjust = -2, size = 5,
            family = "Times New Roman"
          ) +
          scale_x_continuous(
            breaks = custom_breaks(range),
            limits = custom_breaks(range, 2),
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          scale_y_continuous(
            breaks = custom_breaks(c(min(b$lower), max(b$upper))),
            limits = custom_breaks(c(min(b$lower), max(b$upper)), 2),
            labels = label_number(accuracy = 0.01),
            expand = c(0, 0)
          ) +
          # expand_limits(x = 0, y = 0) +
          labs(
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
        TRUE
      },
      .d.render = function(image, ...) {
        return(d.render(image))
      },
      .ols.render = function(image, ...) {
        return(ols.render(image))
      }
    )
  )
}
