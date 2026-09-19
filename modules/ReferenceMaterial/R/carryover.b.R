# This file is a generated template, your changes will not be overwritten

CarryoverClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "CarryoverClass",
    inherit = CarryoverBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        self$results$abb$setContent(
          self$results$abb$content
        )
        vf <- self$data[, self$options$v]
        l <- self$data[, self$options$l]
        vf.g <- split(vf, l)
        v.m.l <- list()
        v.m.m <- list()
        v.m.h <- list()
        v.sd.l <- list()
        v.sd.m <- list()
        v.sd.h <- list()
        J <- self$options$rpd
        I <- length(self$options$v) / J
        for (g in names(vf.g)) {
          K <- nrow(vf.g[[g]])
          if (g == self$options$lf) {
            v.m.l <- lapply(vf.g[[g]], mean)
            v.sd.l <- lapply(vf.g[[g]], sd)
            vw.l <- sum(sapply(vf.g[[g]], function(col) {
              return(sum((col - mean(col))^2))
            })) / I / J / (K - 1)
            vd.l <- 0
            vr.l <- 0
            x.m.m <- mean(as.matrix(vf.g[[g]]))
            for (i in 1:I) {
              if (J > 1) {
                cols <- ((i - 1) * J + 1):(i * J)
                xij <- colMeans(vf.g[[g]][, cols])
                mxi <- mean(as.matrix(vf.g[[g]][, cols]))
              } else {
                xij <- mean(vf.g[[g]][[i]])
                mxi <- xij
              }
              vr.l <- vr.l + sum((xij - mxi)^2)
              vd.l <- vd.l + (mxi - x.m.m)^2
            }
            vd.l <- vd.l / (I - 1)
            if (J > 1) {
              vr.l <- vr.l / I / (J - 1)
            } else {
              vr.l <- 0
            }
          }
          if (g == self$options$mf) {
            v.m.m <- lapply(vf.g[[g]], mean)
            v.sd.m <- lapply(vf.g[[g]], sd)
            vw.m <- sum(sapply(vf.g[[g]], function(col) {
              return(sum((col - mean(col))^2))
            })) / I / J / (K - 1)
            vd.m <- 0
            vr.m <- 0
            x.m.m <- mean(as.matrix(vf.g[[g]]))
            for (i in 1:I) {
              if (J > 1) {
                cols <- ((i - 1) * J + 1):(i * J)
                xij <- colMeans(vf.g[[g]][, cols])
                mxi <- mean(as.matrix(vf.g[[g]][, cols]))
              } else {
                xij <- mean(vf.g[[g]][[i]])
                mxi <- xij
              }
              vr.m <- vr.m + sum((xij - mxi)^2)
              vd.m <- vd.m + (mxi - x.m.m)^2
            }
            vd.m <- vd.m / (I - 1)
            if (J > 1) {
              vr.m <- vr.m / I / (J - 1)
            } else {
              vr.m <- 0
            }
          }
          if (g == self$options$hf) {
            v.m.h <- lapply(vf.g[[g]], mean)
            v.sd.h <- lapply(vf.g[[g]], sd)
            vw.h <- sum(sapply(vf.g[[g]], function(col) {
              return(sum((col - mean(col))^2))
            })) / I / J / (K - 1)
            vd.h <- 0
            vr.h <- 0
            x.m.m <- mean(as.matrix(vf.g[[g]]))
            for (i in 1:I) {
              if (J > 1) {
                cols <- ((i - 1) * J + 1):(i * J)
                xij <- colMeans(vf.g[[g]][, cols])
                mxi <- mean(as.matrix(vf.g[[g]][, cols]))
              } else {
                xij <- mean(vf.g[[g]][[i]])
                mxi <- xij
              }
              vr.h <- vr.h + sum((xij - mxi)^2)
              vd.h <- vd.h + (mxi - x.m.m)^2
            }
            vd.h <- vd.h / (I - 1)
            if (J > 1) {
              vr.h <- vr.h / I / (J - 1)
            } else {
              vr.h <- 0
            }
          }
        }

        cb <- self$results$bg$items$cb
        scb <- self$results$bg$items$scb
        bc <- self$results$bg$items$c
        ci <- self$results$ig$items$ci
        ic <- self$results$ig$items$ic
        ct <- self$results$mr$items$ct
        cvt <- self$results$cv$items$t
        cvc <- self$results$cv$items$c
        summary <- self$results$summary
        for (r in colnames(vf)) {
          cb$addRow(
            rowKey = r,
            values = list(
              r = r,
              lsd = v.sd.l[[r]],
              lm = v.m.l[[r]],
              msd = v.sd.m[[r]],
              mm = v.m.m[[r]],
              hsd = v.sd.h[[r]],
              hm = v.m.h[[r]]
            )
          )
        }
        wi <- list()
        wo <- list()
        lv <- list(
          l = self$options$lf,
          m = mean(unlist(v.m.l)),
          a = abs(self$options$lv)
        )
        lv$b <- lv$m - lv$a
        lv$ab <- self$options$lb
        lv$w <- ifelse(abs(lv$b) > lv$ab, "No", "Yes")
        if (lv$w == "Yes") {
          wi[["low"]] <- TRUE
        } else {
          wo[["low"]] <- TRUE
        }
        scb$addRow(
          rowKey = "l",
          values = lv
        )
        mv <- list(
          l = self$options$mf,
          m = mean(unlist(v.m.m)),
          a = abs(self$options$mv)
        )
        mv$b <- mv$m - mv$a
        mv$ab <- self$options$mb
        mv$w <- ifelse(abs(mv$b) > mv$ab, "No", "Yes")
        if (mv$w == "Yes") {
          wi[["medium"]] <- TRUE
        } else {
          wo[["medium"]] <- TRUE
        }
        scb$addRow(
          rowKey = "m",
          values = mv
        )
        hv <- list(
          l = self$options$hf,
          m = mean(unlist(v.m.h)),
          a = abs(self$options$hv)
        )
        hv$b <- hv$m - hv$a
        hv$ab <- self$options$hb
        hv$w <- ifelse(abs(hv$b) > hv$ab, "No", "Yes")
        if (hv$w == "Yes") {
          wi[["high"]] <- TRUE
        } else {
          wo[["high"]] <- TRUE
        }
        scb$addRow(
          rowKey = "h",
          values = hv
        )
        if (length(wi) > 0 && length(wo) > 0) {
          bcc <- tprintf(
            "Level %s %s within allowable bias, however level %s %s without allowable bias.",
            join(names(wi)), ifelse(length(wi) > 1, "were", "was"),
            join(names(wo)), ifelse(length(wo) > 1, "were", "was")
          )
        } else if (length(wi) > 0) {
          bcc <- tprintf("All levels were within allowable bias.")
        } else if (length(wo) > 0) {
          bcc <- tprintf("All levels were without allowable bias.")
        }
        bc$setContent(bcc)
        vw <- list(
          v = tprintf("($V_W$) Pooled within-run variance"),
          l = sum(unlist(v.sd.l)^2) / length(v.sd.l),
          m = sum(unlist(v.sd.m)^2) / length(v.sd.m),
          h = sum(unlist(v.sd.h)^2) / length(v.sd.h)
        )
        ci$addRow(rowKey = "vw", values = vw)
        vm <- list(
          v = tprintf("($V_M$) Variance of run means"),
          l = var(unlist(v.m.l)),
          m = var(unlist(v.m.m)),
          h = var(unlist(v.m.h))
        )
        ci$addRow(rowKey = "vm", values = vm)
        vd <- list(
          v = tprintf("($V_D$) Adjusted between-run variance<br>($V_M$-$V_W$/3)"),
          l = ifelse(vm$l - vw$l / 3 > 0, vm$l - vw$l / 3, 0),
          m = ifelse(vm$m - vw$m / 3 > 0, vm$m - vw$m / 3, 0),
          h = ifelse(vm$h - vw$h / 3 > 0, vm$h - vw$h / 3, 0)
        )

        ci$addRow(rowKey = "vd", values = vd)
        vt <- list(
          v = tprintf("($V_T$) Combined variance<br>($V_W$+$V_D$)"),
          l = vw$l + vd$l,
          m = vw$m + vd$m,
          h = vw$h + vd$h
        )
        ci$addRow(rowKey = "vt", values = vt)
        s <- list(
          v = tprintf("($S$) Total standard deviation<br>($\\sqrt{V_T}$)"),
          l = sqrt(vt$l),
          m = sqrt(vt$m),
          h = sqrt(vt$h)
        )
        ci$addRow(rowKey = "s", values = s)
        m <- list(
          v = tprintf("($M$) Grand mean value"),
          l = mean(unlist(v.m.l)),
          m = mean(unlist(v.m.m)),
          h = mean(unlist(v.m.h))
        )
        ci$addRow(rowKey = "m", values = m)
        c <- list(
          v = tprintf("($C$) Combined $CV\\%%$<br>($S/M\\cdot 100\\%%$)"),
          l = 100 * s$l / m$l,
          m = 100 * s$m / m$m,
          h = 100 * s$h / m$h
        )
        ci$addRow(rowKey = "c", values = c)
        a <- list(
          v = tprintf("Allowable imprecision $CV\\%%$"),
          l = self$options$lcv,
          m = self$options$mcv,
          h = self$options$hcv
        )
        ci$addRow(rowKey = "a", values = a)
        ar <- list(
          v = "Accept or reject",
          l = ifelse(c$l < a$l, "Accept", "Reject"),
          m = ifelse(c$m < a$m, "Accept", "Reject"),
          h = ifelse(c$h < a$h, "Accept", "Reject")
        )
        ci$addRow(rowKey = "ar", values = ar)
        x_j <- sapply(l, function(item) {
          if (item == self$options$lf) {
            return(-1)
          } else if (item == self$options$mf) {
            return(0)
          } else if (item == self$options$hf) {
            return(1)
          } else {
            stop(tprintf("Unknown level %s.", l))
          }
        })
        wi <- list()
        wo <- list()
        if (c$l < a$l) {
          wi[["low"]] <- TRUE
        } else {
          wo[["low"]] <- TRUE
        }
        if (c$m < a$m) {
          wi[["medium"]] <- TRUE
        } else {
          wo[["medium"]] <- TRUE
        }
        if (c$h < a$h) {
          wi[["high"]] <- TRUE
        } else {
          wo[["high"]] <- TRUE
        }
        if (length(wi) > 0 && length(wo) > 0) {
          icc <- tprintf(
            "Level %s %s within allowable imprecision, however level %s %s without allowable imprecision.",
            join(names(wi)), ifelse(length(wi) > 1, "were", "was"),
            join(names(wo)), ifelse(length(wo) > 1, "were", "was")
          )
        } else if (length(wi) > 0) {
          icc <- tprintf("All levels were within allowable imprecision.")
        } else if (length(wo) > 0) {
          icc <- tprintf("All levels were without allowable imprecision.")
        }
        ic$setContent(icc)
        n <- length(x_j)
        x_prev <- c(0, x_j[1:(n - 1)])
        time_centered <- (-(n - 1) / 2):((n - 1) / 2)
        if (length(time_centered) != n) {
          stop(tprintf("Number of samples (%d) must be odd.", n))
        }
        g_i <- time_centered
        x_sq_adj <- x_j^2 - 2 / 3
        for (i in 1:n) {
          ct$addRow(
            rowKey = i,
            values = list(
              seq = i,
              l = as.character(l[[i]]),
              c = x_j[[i]],
              sqadj = x_sq_adj[[i]],
              t = time_centered[[i]],
              co = x_prev[[i]]
            )
          )
        }
        v.model <- list()
        v.b <- list()
        v.b0.a <- list()
        v.b1.a <- list()
        v.b2.a <- list()
        v.b3.a <- list()
        v.b4.a <- list()

        v.se <- list()
        v.t <- list()
        syx <- list()
        rp <- self$results$mr$items$rp
        rpc <- self$results$mr$items$rpc
        mrs <- self$results$mr$items$mrs
        ts <- self$results$t$items$ts
        tc <- self$results$t$items$c
        st <- self$results$t$items$st
        sf <- self$options$mv - self$options$lv
        for (r in colnames(vf)) {
          data <- data.frame(
            Y = vf[[r]],
            x_j = x_j,
            x_prev = x_prev,
            x_sq_adj = x_sq_adj,
            g_i = g_i
          )

          v.model[[r]] <- lm(Y ~ x_j + x_prev + x_sq_adj + g_i, data = data)
          coef <- coefficients(v.model[[r]])
          syx[[r]] <- summary(v.model[[r]])$sigma
          cse <- coef(summary(v.model[[r]]))[, "Std. Error"]
          v.b[[r]] <- c(
            coef["(Intercept)"],
            coef["x_j"],
            coef["x_prev"],
            coef["x_sq_adj"],
            coef["g_i"]
          )
          v.se[[r]] <- c(
            cse["(Intercept)"],
            cse["x_j"],
            cse["x_prev"],
            cse["x_sq_adj"],
            cse["g_i"]
          )
          rpl <- list(
            r = r,
            b0 = v.b[[r]][1],
            b1 = v.b[[r]][2],
            b2 = v.b[[r]][3],
            b3 = v.b[[r]][4],
            b4 = v.b[[r]][5],
            syx = syx[[r]]
          )

          v.b1.a[[r]] <- rpl$b1a <- rpl$b1 / sf
          v.b0.a[[r]] <- rpl$b0a <- rpl$b0 - rpl$b1a * self$options$mv
          v.b2.a[[r]] <- rpl$b2a <- rpl$b2 / rpl$b1 * 100
          v.b3.a[[r]] <- rpl$b3a <- rpl$b3 / sf^2
          v.b4.a[[r]] <- rpl$b4a <- rpl$b4
          v.t[[r]] <- c(
            v.b0.a[[r]] / cse[[1]],
            (v.b1.a[[r]] - 1) / cse[[2]] * sf,
            rpl$b2 / cse[[3]],
            v.b3.a[[r]] / cse[[4]] * sf^2,
            v.b4.a[[r]] / cse[[5]]
          )
          rp$addRow(
            rowKey = r,
            values = rpl
          )
          mrs$addRow(
            rowKey = r,
            values = rpl
          )
          ts$addRow(
            rowKey = r,
            values = list(
              r = r,
              t0 = v.t[[r]][[1]],
              se0 = v.se[[r]][1],
              t1 = v.t[[r]][[2]],
              se1 = v.se[[r]][2],
              t2 = v.t[[r]][[3]],
              se2 = v.se[[r]][3],
              t3 = v.t[[r]][[4]],
              se3 = v.se[[r]][4],
              t4 = v.t[[r]][[5]],
              se4 = v.se[[r]][5]
            )
          )
        }
        s.v <- list(
          r = "Summary",
          b0a = mean(unlist(v.b0.a)),
          b1a = mean(unlist(v.b1.a)),
          b2a = mean(unlist(v.b2.a)),
          b3a = mean(unlist(v.b3.a)),
          b4a = mean(unlist(v.b4.a)),
          syx = mean(unlist(syx))
        )
        mrs$addRow(
          rowKey = "Summary",
          values = s.v
        )
        ts$setNote(
          "n1",
          tprintf(
            paste0(
              "$t_0=B_{0adj}/SE_0$; $t_1=S_f \\cdot (B_{1adj}-1)/SE_1$; ",
              "$t_2=B_{2adj}/SE_2$; $t_3=S_f^2 \\cdot B_{3adj}/SE_3$; $t_4=B_{4adj}/SE_4$."
            )
          )
        )
        tc$setContent(
          c(
            tprintf(
              "$t$ for 4 degrees of freedom is significant ($p<%s$) if $t>%s$ or $t<%s$. ",
              ftrim(self$options$p), ftrim(qt(1 - self$options$p / 2, 4)), ftrim(-qt(1 - self$options$p / 2, 4))
            ),
            "If any of the $t$ statistics for drift, carryover, or nonlinearity are found significant, it should be determined whether the same problem recurs in other runs.",
            "Usually however, if the problem appears to be limited only to one run, it may be safely ignored."
          )
        )
        rpc$setContent(
          c(
            tprintf("Multiple regression model for each run where $\\hat{Y}$ is the estimated measured value:"),
            tprintf("$\\hat{Y} = B_0 + B_1 \\cdot \\text{Coded} + B_2 \\cdot \\text{Carryover} + B_3 \\cdot \\text{Adjusted} + B_4 \\cdot \\text{Time}$"),
            tprintf(
              "Scale factor $S_f$ = mid ref. - low ref. =  %s - %s = %s",
              ftrim(self$options$mv), ftrim(self$options$lv), ftrim(sf)
            ),
            tprintf("$B_{1adj} = B_1/S_f$"),
            tprintf("$B_{0adj} = B_0 - B_{1adj} \\cdot \\text{mid ref.}$"),
            tprintf("$B_{2adj} = B_2/B_1\\cdot 100$"),
            tprintf("$B_{3adj} = B_3/S_f^2$"),
            tprintf("$B_{4adj} = B_4$")
          )
        )
        stv <- list(
          rp = "Intercept",
          p = sum(unlist(v.b0.a) > 0),
          n = sum(unlist(v.b0.a) < 0)
        )
        stv$s <- ifelse(stv$p == 0 || stv$n == 0, "Yes", "No")
        st$addRow(
          rowKey = "b0",
          values = stv
        )
        stv <- list(
          rp = "Slope $-1$",
          p = sum(unlist(v.b1.a) - 1 > 0),
          n = sum(unlist(v.b1.a) - 1 < 0)
        )
        stv$s <- ifelse(stv$p == 0 || stv$n == 0, "Yes", "No")
        st$addRow(
          rowKey = "b1",
          values = stv
        )
        stv <- list(
          rp = "%Carryover",
          p = sum(unlist(v.b2.a) > 0),
          n = sum(unlist(v.b2.a) < 0)
        )
        stv$s <- ifelse(stv$p == 0 || stv$n == 0, "Yes", "No")
        st$addRow(
          rowKey = "b2",
          values = stv
        )
        stv <- list(
          rp = "Nonlinearity",
          p = sum(unlist(v.b3.a) > 0),
          n = sum(unlist(v.b3.a) < 0)
        )
        stv$s <- ifelse(stv$p == 0 || stv$n == 0, "Yes", "No")
        st$addRow(
          rowKey = "b3",
          values = stv
        )
        stv <- list(
          rp = "Drift",
          p = sum(unlist(v.b4.a) > 0),
          n = sum(unlist(v.b4.a) < 0)
        )
        stv$s <- ifelse(stv$p == 0 || stv$n == 0, "Yes", "No")
        st$addRow(
          rowKey = "b4",
          values = stv
        )
        evw.l <- vw.l
        evr.l <- vr.l - vw.l / K
        if (evr.l < 0) evr.l <- 0
        evd.l <- ifelse(J > 1, vd.l - vr.l / J, vd.l - vw.l / K)
        if (evd.l < 0) evd.l <- 0
        evt.l <- evd.l + evr.l + evw.l
        evw.m <- vw.m
        evr.m <- vr.m - vw.m / K
        if (evr.m < 0) evr.m <- 0
        evd.m <- ifelse(J > 1, vd.m - vr.m / J, vd.m - vw.m / K)
        if (evd.m < 0) evd.m <- 0
        evt.m <- evd.m + evr.m + evw.m
        evw.h <- vw.h
        evr.h <- vr.h - vw.h / K
        if (evr.h < 0) evr.h <- 0
        evd.h <- ifelse(J > 1, vd.h - vr.h / J, vd.h - vw.h / K)
        if (evd.h < 0) evd.h <- 0
        evt.h <- evd.h + evr.h + evw.h
        cvt$addRow(
          rowKey = "wr",
          values = list(
            d = 'Within-run<br>("unadjusted")',
            f = tprintf("$V_W = \\sum_i\\sum_j\\sum_k(x_{ijk}-\\overline{x}_{ij})^2/[IJ(K-1)]$"),
            lv = vw.l,
            mv = vw.m,
            hv = vw.h
          )
        )
        cvt$addRow(
          rowKey = "br",
          values = list(
            d = 'Between-run<br>("unadjusted")',
            f = tprintf("$V_R = \\sum_i\\sum_j(\\overline{x}_{ij}-\\overline{x}_{i})^2/[I(J-1)]$"),
            lv = vr.l,
            mv = vr.m,
            hv = vr.h
          )
        )
        cvt$addRow(
          rowKey = "bd",
          values = list(
            d = 'Between-day<br>("unadjusted")',
            f = tprintf("$V_D = \\sum_i(\\overline{x}_{i}-\\overline{x})^2/(I-1)$"),
            lv = vd.l,
            mv = vd.m,
            hv = vd.h
          )
        )
        cvt$addRow(
          rowKey = "wre",
          values = list(
            d = "Within-run<br>estimate",
            f = tprintf("$\\hat{\\sigma}_W^2 = V_W$"),
            lv = evw.l,
            mv = evw.m,
            hv = evw.h
          )
        )
        cvt$addRow(
          rowKey = "bre",
          values = list(
            d = "Between-run<br>estimate",
            f = tprintf("$\\hat{\\sigma}_R^2 = V_R - V_W/K$"),
            lv = evr.l,
            mv = evr.m,
            hv = evr.h
          )
        )
        cvt$addRow(
          rowKey = "bde",
          values = list(
            d = "Between-day<br>estimate",
            f = tprintf("$\\hat{\\sigma}_D^2 = V_D - V_R/J$"),
            lv = evd.l,
            mv = evd.m,
            hv = evd.h
          )
        )
        cvt$addRow(
          rowKey = "tav",
          values = list(
            d = "Total adjusted<br>variance",
            f = tprintf("$\\hat{\\sigma}_T^2 = \\hat{\\sigma}_D^2+\\hat{\\sigma}_R^2+\\hat{\\sigma}_W^2$"),
            lv = evt.l,
            mv = evt.m,
            hv = evt.h
          )
        )
        cvc$setContent(
          c(
            "Where:",
            tprintf("\t$I=%d$: number of days;", as.integer(I)),
            tprintf("\t$J=%d$: number of runs on each day;", J),
            tprintf("\t$K=%d$: number of observations in each run;", K),
            tprintf("\t$x_{ijk}$: observation $k$ in run $j$ on day $i$;"),
            tprintf("\t$\\overline{x}_{ij}$: mean of the $K$ observations in run $j$ on day $i$;"),
            tprintf("\t$\\overline{x}_{i}$: mean of all observations on day $i$;"),
            tprintf("\t$\\overline{x}$: grand mean of all observations.")
          )
        )
        summary$setContent(
          tprintf(
            "The summary carryover: $B_{2adj} \\%% = %s\\%%.$",
            ftrim(s.v$b2a)
          )
        )
      }
    )
  )
}
