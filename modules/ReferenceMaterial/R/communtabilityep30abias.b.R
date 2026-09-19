# This file is a generated template, your changes will not be overwritten
library(ggplot2)
CommuntabilityEP30ABiasClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "CommuntabilityEP30ABiasClass",
    inherit = CommuntabilityEP30ABiasBase,
    private = list(
      .pp.mp.render = function(image, ...) {
        if (is.null(image$state$x)) {
          return(FALSE)
        }
        x <- image$state$x
        y <- image$state$y
        xlab <- image$state$xlab
        ylab <- image$state$ylab
        df <- data.frame(x = x, y = y)
        t <- theme(
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
          axis.text.y = element_text(size = 14, family = "Times New Roman"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
        )
        p <- ggplot(
          df, aes(x = x, y = y)
        ) +
          geom_point(shape = 18, color = "black", fill = "black", size = 4) +
          labs(x = xlab, y = ylab) +
          theme_minimal() +
          t

        if (self$options$logscale) {
          p <- p + scale_x_log10()
        }
        print(p)
        TRUE
      },
      .ci.render = function(image, ...) {
        if (is.null(image$state$C)) {
          return(FALSE)
        }
        df_black <- data.frame(
          x = image$state$cs.x,
          y = image$state$cs.y
        )
        df_red <- data.frame(
          x = image$state$rm.x,
          y = image$state$rm.y,
          ymin = image$state$rm.l,
          ymax = image$state$rm.h,
          RM_ID = image$state$rm.id
        )
        hline_df <- data.frame(
          yintercept = c(
            image$state$bcs - image$state$C,
            image$state$bcs + image$state$C
          ),
          LineType = c("C", "C"),
          LineColor = c("C", "C")
        )
        bcs_line <- data.frame(
          yintercept = c(
            image$state$bcs
          ),
          LineType = c("B_{CS}")
        )
        r <- max(image$state$cs.x) - min(image$state$cs.x)
        if (self$options$logscale) {
          r <- log10(max(image$state$cs.x)) - log10(min(image$state$cs.x))
        }
        plot <- ggplot() +
          geom_point(data = df_black, aes(x = x, y = y, shape = "CS"), color = "black", size = 2.5) +
          geom_errorbar(
            data = df_red, aes(x = x, y = y, ymin = ymin, ymax = ymax), color = "red",
            width = 0.02 * r, size = 0.2
          ) +
          geom_point(data = df_red, aes(x = x, y = y, shape = "RM"), color = "red", size = 2.5) +
          geom_text_repel(
            data = df_red, aes(x = x, y = y, label = as.character(RM_ID)),
            segment.color = NA,
            color = "#e00f77", hjust = -0.7, vjust = -0.5,
            size = 5, family = "Times New Roman"
          ) +
          geom_hline(
            data = hline_df,
            aes(yintercept = yintercept, linetype = LineType),
            size = 0.5,
            color = "red"
          ) +
          geom_hline(
            data = bcs_line,
            aes(yintercept = yintercept, linetype = LineType),
            size = 0.5,
            color = "black"
          )
        if (self$options$logscale) {
          plot <- plot + scale_x_log10()
        }
        plot <- plot +
          scale_shape_manual(name = "Point Type", values = c("CS" = 18, "RM" = 15)) +
          scale_linetype_manual(
            name = "Line Type", values = c("B_{CS}" = "solid", "C" = "dashed"),
            labels = c(expression(B[CS]), "C")
          ) +
          guides(
            color = "none",
            shape = guide_legend("Shape Type"),
            linetype = guide_legend("Line Type")
          ) +
          theme_bw() +
          theme(
            panel.grid = element_blank(),
            panel.border = element_rect(color = "black", fill = NA),
            # Adjust axis title font size
            axis.title.x = element_text(size = 14, family = "Times New Roman"),
            axis.title.y = element_text(size = 14, family = "Times New Roman"),

            # Adjust axis tick labels font size
            axis.text.x = element_text(size = 14, family = "Times New Roman"),
            axis.text.y = element_text(size = 14, family = "Times New Roman"),

            # Adjust legend title and text font size
            legend.title = element_text(size = 14, family = "Times New Roman"),
            legend.text = element_text(size = 14, family = "Times New Roman")
          ) +
          labs(
            x = "MP x concentration",
            y = "Bias"
          )
        print(plot)
        TRUE
      },
      .run = function() {
        self$results$mr$setContent(
          c(
            "Characterization and Qualification of Commutable Reference Materials for Laboratory Medicine",
            "CLSI, EP30-2nd Edition",
            "August 2024"
          )
        )

        welch_df <- function(s1, n1, s2, n2) {
          num <- (s1^2 / n1 + s2^2 / n2)^2
          denom <- ((s1^2 / n1)^2) / (n1 - 1) + ((s2^2 / n2)^2) / (n2 - 1)
          df <- as.integer(num / denom)
          return(df)
        }
        texformat <- function(str) {
          gsub("([{}])", "\\1", str)
        }
        rm.cv <- function(prepare.data) {
          pd <- prepare.data
          k <- pd$k
          mean.tot.x <- lapply(pd$rm.x.g, function(group) {
            mean(
              unlist(apply(group, 1, function(row) mean(unlist(row), na.rm = TRUE)))
            )
          })
          s.posmean.x <- lapply(pd$rm.x.g, function(group) {
            v <- unlist(apply(group, 1, function(row) mean(unlist(row), na.rm = TRUE)))
            if (length(v) > 1) {
              return(sd(v))
            }
            return(0)
          })
          s.e.x <- lapply(pd$rm.x.g, function(group) {
            v <- unlist(apply(group, 1, function(row) sd(unlist(row), na.rm = TRUE)))
            sqrt(sum(v^2) / length(v))
          })
          mean.tot.y <- lapply(pd$rm.y.g, function(group) {
            mean(
              unlist(apply(group, 1, function(row) mean(unlist(row), na.rm = TRUE)))
            )
          })
          s.posmean.y <- lapply(pd$rm.y.g, function(group) {
            v <- unlist(apply(group, 1, function(row) mean(unlist(row), na.rm = TRUE)))
            if (length(v) > 1) {
              return(sd(v))
            }
            return(0)
          })
          s.e.y <- lapply(pd$rm.y.g, function(group) {
            v <- unlist(apply(group, 1, function(row) sd(unlist(row), na.rm = TRUE)))
            sqrt(sum(v^2) / length(v))
          })
          F.x <- setNames(lapply(pd$rm.g.u, function(g) {
            k * s.posmean.x[[g]]^2 / s.e.x[[g]]^2
          }), pd$rm.g.u)
          F.y <- setNames(lapply(pd$rm.g.u, function(g) {
            k * s.posmean.y[[g]]^2 / s.e.y[[g]]^2
          }), pd$rm.g.u)
          s.pos.x2 <- setNames(lapply(pd$rm.g.u, function(g) {
            s.posmean.x[[g]]^2 - s.e.x[[g]]^2 / k
          }), pd$rm.g.u)
          s.pos.y2 <- setNames(lapply(pd$rm.g.u, function(g) {
            s.posmean.y[[g]]^2 - s.e.y[[g]]^2 / k
          }), pd$rm.g.u)
          df1 <- pd$rm.pos.n - 1
          df2 <- (pd$k - 1) * pd$rm.pos.n
          F.c <- qf(1 - pd$alpha, df1, df2)
          pos.x.effect <- F.x > F.c
          pos.y.effect <- F.y > F.c
          pooled.s.posmean.x2 <- mean(unlist(s.posmean.x)^2)
          pooled.s.posmean.y2 <- mean(unlist(s.posmean.y)^2)
          pooled.s.e.x2 <- mean(unlist(s.e.x)^2)
          pooled.s.e.y2 <- mean(unlist(s.e.y)^2)
          pooled.s.e2 <- pooled.s.e.x2 + pooled.s.e.y2
          pooled.s.pos.x2 <- pooled.s.posmean.x2 - pooled.s.e.x2 / k
          pooled.s.pos.y2 <- pooled.s.posmean.y2 - pooled.s.e.y2 / k
          return(list(
            s.pos.mean.x = s.posmean.x,
            s.pos.mean.y = s.posmean.y,
            s.e.x = s.e.x,
            s.e.y = s.e.y,
            F.x = F.x,
            F.y = F.y,
            df1 = df1,
            df2 = df2,
            F.c = F.c,
            s.pos.x2 = s.pos.x2,
            s.pos.y2 = s.pos.y2,
            pos.x.effect = pos.x.effect,
            pos.y.effect = pos.y.effect,
            pooled.s.posmean.x2 = pooled.s.posmean.x2,
            pooled.s.posmean.y2 = pooled.s.posmean.y2,
            pooled.s.e.x2 = pooled.s.e.x2,
            pooled.s.e.y2 = pooled.s.e.y2,
            pooled.s.e2 = pooled.s.e2,
            pooled.s.pos.x2 = pooled.s.pos.x2,
            pooled.s.pos.y2 = pooled.s.pos.y2,
            mean.tot.x = mean.tot.x,
            mean.tot.y = mean.tot.y
          ))
        }
        sB <- function(b, xy.m, q) {
          indices <- order(xy.m)
          b <- b[indices]
          Bi <- diff(b)
          B_CS <- mean(b)
          return(
            sqrt(1 / (q - 1) * sum((Bi - B_CS)^2))
          )
        }
        rmcs.d <- function(prepare.data, rm.cv.data) {
          pd <- prepare.data
          rd <- rm.cv.data
          cs.b <- na.omit(unlist(pd$cs.b))
          cs.x.m <- na.omit(unlist(pd$cs.x.m))
          indices <- order(cs.x.m)
          cs.b <- cs.b[indices]
          cs.x.m <- cs.x.m[indices]

          b.u <- lm(cs.b ~ cs.x.m)
          intercept <- coef(b.u)[[1]]
          slope <- coef(b.u)[[2]]
          corr <- cor(cs.b, cs.x.m, method = "pearson")
          cs.v.x <- cs.x.m
          cs.v.y <- cs.b - (intercept + slope * cs.v.x)
          rm.v.x <- unlist(pd$rm.x.g.m)
          rm.v.y <- unlist(pd$rm.y.g.m) - rm.v.x
          rm.v.y.adjust <- rm.v.y - (intercept + slope * rm.v.x)
          Bi <- diff(cs.v.y)
          B_CS <- mean(cs.b)
          s_MSSD <- sqrt(1 / 2 / pd$n * sum(Bi^2))
          s_B <- sd(cs.v.y)
          SD <- sqrt(1 / (pd$n + 1) * (1 - 1 / (pd$n - 1)))
          Q <- s_MSSD^2 / s_B^2
          Q.c <- qnorm(pd$alpha, mean = 1, sd = SD)
          trend <- Q < Q.c
          F <- pd$k * s_MSSD^2 / (pd$cs.sx^2 + pd$cs.sy^2)
          df1 <- as.integer(pd$n / 2)
          df2 <- welch_df(pd$cs.sx, pd$n, pd$cs.sy, pd$n)
          F.c <- qf(1 - pd$alpha, df1, df2)
          xy.diff <- F > F.c
          s_d2 <- s_MSSD^2 - (pd$cs.sx^2 + pd$cs.sy^2) / pd$k
          if (!trend) {
            s_d2 <- s_B^2 - (pd$cs.sx^2 + pd$cs.sy^2) / pd$k
          }
          s_d_corr <- sqrt(s_d2 - rd$pooled.s.pos.x2 - rd$pooled.s.pos.y2)
          s.e2.cs <- pd$cs.sx^2 + pd$cs.sy^2
          s.e2.rm <- rd$pooled.s.e2
          df.cs <- pd$n * (pd$cs.k - 1)
          df.rm <- length(pd$rm.id.u) * pd$p * (pd$k - 1)
          if (s.e2.cs > s.e2.rm) {
            F.csrm <- s.e2.cs / s.e2.rm
            F.c.csrm <- qf(1 - pd$alpha / 2, df.cs, df.rm)
          } else {
            F.csrm <- s.e2.rm / s.e2.cs
            F.c.csrm <- qf(1 - pd$alpha / 2, df.rm, df.cs)
          }
          s.diff.csrm <- FALSE
          if (F.csrm > F.c.csrm) {
            s.diff.csrm <- TRUE
          }
          return(
            list(
              cs.b = cs.b,
              b.u = b.u,
              corr = corr,
              slope = slope,
              intercept = intercept,
              cs.v.x = cs.v.x,
              cs.v.y.adjust = cs.v.y,
              rm.v.x = rm.v.x,
              rm.v.y = rm.v.y,
              rm.v.y.adjust = rm.v.y.adjust,
              Bi = Bi,
              B_CS = B_CS,
              s_MSSD = s_MSSD,
              s_B = s_B,
              SD = SD,
              Q = Q,
              Q.c = Q.c,
              trend = trend,
              F = F,
              df1 = df1,
              df2 = df2,
              F.c = F.c,
              s.e2.cs = s.e2.cs,
              s.e2.rm = s.e2.rm,
              F.csrm = F.csrm,
              F.c.csrm = F.c.csrm,
              df.cs = df.cs,
              df.rm = df.rm,
              s.diff.csrm = s.diff.csrm,
              xy.diff = xy.diff,
              s_d2 = s_d2,
              s_d_corr = s_d_corr
            )
          )
        }
        get_cs_indices <- function(cs, rm) {
          rm_min <- min(rm)
          rm_max <- max(rm)
          l <- max(cs[which(cs < rm_min)])
          r <- min(cs[which(cs > rm_max)])
          return(which(cs > l & cs < r))
        }
        is.constant <- function(model.summary, alpha = 0.05) {
          coefs <- coef(model.summary)
          if (nrow(coefs) < 2) {
            return(TRUE)
          }
          return(all(coefs[-1, "Pr(>|t|)"] > alpha))
        }
        is.linear <- function(model.summary, alpha) {
          p.value <- coef(model.summary)[-1, "Pr(>|t|)"]
          return(p.value < alpha)
        }
        rm.c <- function(prepare.data, rm.cv.data, rmcs.d.data) {
          pd <- prepare.data
          rcd <- rm.cv.data
          rdd <- rmcs.d.data
          alpha <- pd$alpha
          e.f <- pd$e.f
          rm.ref <- rdd$rm.v.x
          rm.m <- unlist(pd$rm.y.g.m)

          S_RM.x <- sqrt(sum(unlist(pd$rm.x.g.sd)^2) / pd$k)
          S_RM.y <- sqrt(sum(unlist(pd$rm.y.g.sd)^2) / pd$k)
          uB_RM <- sqrt((S_RM.x^2 + S_RM.y^2) / (pd$k * pd$p))
          B_CS <- rdd$B_CS
          uB_CS <- rdd$s_B / sqrt(pd$n)
          q <- pd$n
          ud_RM <- sqrt(uB_RM^2 + uB_CS^2)
          U.d.RM <- ud_RM * e.f

          rm.l <- rm.m - U.d.RM
          rm.u <- rm.m + U.d.RM
          rm.y.l <- rm.l - rm.ref
          rm.y.u <- rm.u - rm.ref
          rm.y.l.adjust <- rm.y.l - (rdd$intercept + rdd$slope * rdd$rm.v.x)
          rm.y.u.adjust <- rm.y.u - (rdd$intercept + rdd$slope * rdd$rm.v.x)
          # rm.y.l <- rdd$rm.v.y - (rdd$intercept + rdd$slope * rdd$rm.v.x)
          # rm.y.u <- rdd$rm.v.y + (rdd$intercept + rdd$slope * rdd$rm.v.x)

          C <- pd$C
          commutable <- setNames(
            sapply(pd$rm.id.u, function(rmid) {
              if (abs(rm.y.l.adjust[[rmid]]) <= C && abs(rm.y.u.adjust[[rmid]]) <= C) {
                return("Commutable")
              } else if (
                abs(rm.y.l.adjust[[rmid]]) <= C || abs(rm.y.u.adjust[[rmid]]) <= C
              ) {
                if (abs(rdd$rm.v.y.adjust[[rmid]]) <= C) {
                  return("Indeterminate<br>(mean within criterion)")
                } else {
                  return("Indeterminate<br>(mean beyond criterion)")
                }
              } else {
                return("Non-commutable")
              }
            }),
            pd$rm.id.u
          )
          return(list(
            C = C,
            rm.ref = rm.ref,
            rm.m = rm.m,
            rm.l = rm.l,
            rm.u = rm.u,
            rm.y.l = rm.y.l,
            rm.y.u = rm.y.u,
            rm.y.l.adjust = rm.y.l.adjust,
            rm.y.u.adjust = rm.y.u.adjust,
            S_RM.x = S_RM.x,
            S_RM.y = S_RM.y,
            B.CS = B_CS,
            u.B.CS = uB_CS,
            u.d.RM = ud_RM,
            u.B.RM = uB_RM,
            U.d.RM = U.d.RM,
            s.B = rdd$s_B,
            s.MSSD = rdd$s_MSSD,
            q = q,
            commutable = commutable
          ))
        }
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)

        pd <- bias.prepare.data(self$data, self$options)
        ol <- self$results$descriptive$items$o$items
        if (length(pd$cs.outliers) > 0) {
          ol$cs$setVisible(TRUE)
          for (cso in names(pd$cs.outliers)) {
            ol$cs$addRow(
              rowKey = cso,
              values = list(cs.id = cso, v = pd$cs.outliers[[cso]])
            )
          }
          ol$csc$setContent(
            paste0(
              tprintf(
                "According to the Grubbs's test with $G_{(%s,%d)}$, ",
                ftrim(pd$g.alpha), pd$cs.g.n
              ),
              tprintf(
                "ID %s %s.",
                join(names(pd$cs.outliers)),
                ifelse(length(pd$cs.outliers) > 1,
                  "have outliers", "has outlier"
                )
              )
            )
          )
        } else {
          ol$csc$setContent("Non-declared outliers.")
        }
        if (length(pd$rm.outliers) > 0) {
          ol$rm$setVisible(TRUE)
          for (rmo in names(pd$rm.outliers)) {
            ol$rm$addRow(
              rowKey = rmo,
              values = list(
                rm.id = rmo,
                pos = pd$rm.outliers[[rmo]]$p,
                v = pd$rm.outliers[[rmo]]$v
              )
            )
          }
          ol$rmc$setContent(
            paste0(
              tprintf(
                "According to the Grubbs's test with $G_{(%s,%d)}$, ",
                ftrim(pd$g.alpha), pd$rm.g.n
              ),
              tprintf(
                "ID %s %s.",
                join(names(pd$rm.outliers)),
                ifelse(length(pd$rm.outliers) > 1,
                  "have outliers", "has outlier"
                )
              )
            )
          )
        } else {
          ol$rmc$setContent("Non-declared outliers.")
        }
        mc <- self$results$descriptive$items$mc$items
        for (rm.id in pd$rm.id.u) {
          mc$rm$addRow(
            rowKey = rm.id,
            values = list(
              rm.id = rm.id,
              mx = pd$rm.x.g.m[[rm.id]],
              my = pd$rm.y.g.m[[rm.id]],
              sdx = pd$rm.x.g.sd[[rm.id]],
              sdy = pd$rm.y.g.sd[[rm.id]],
              xy = pd$rm.x.g.m[[rm.id]] / 2 + pd$rm.y.g.m[[rm.id]] / 2
            )
          )
        }
        for (cs.id in pd$cs.id.u) {
          mc$cs$addRow(
            rowKey = cs.id,
            values = list(
              cs.id = cs.id,
              mx = pd$cs.x.m[[cs.id]],
              my = pd$cs.y.m[[cs.id]],
              sdx = pd$cs.x.sd[[cs.id]],
              sdy = pd$cs.y.sd[[cs.id]],
              xy = pd$cs.xy.m[[cs.id]],
              b = pd$cs.b[[cs.id]]
            )
          )
        }
        mc$c$setContent(c(
          tprintf("Number of clinical samples: $N_{CS}=%d$", pd$n),
          tprintf(
            "Standard deviation of clinical samples on measurement procedure X: $s_X=%s$",
            ftrim(pd$cs.sx)
          ),
          tprintf(
            "Standard deviation of clinical samples on measurement procedure Y: $s_Y=%s$",
            ftrim(pd$cs.sy)
          )
        ))
        v <- self$results$descriptive$items$v$items
        xxlabel <- "Measurement procedure X"
        xylabel <- "Within-subject standard deviation"
        yxlabel <- "Measurement procedure Y"
        yylabel <- "Within-subject standard deviation"
        dxlabel <- "(X+Y)/2"
        dylabel <- "Y-X"
        if (pd$unit != "") {
          xxlabel <- paste0(xxlabel, " (", pd$unit, ")")
          xylabel <- paste0(xylabel, " (", pd$unit, ")")
          yxlabel <- paste0(yxlabel, " (", pd$unit, ")")
          yylabel <- paste0(yylabel, " (", pd$unit, ")")
          dxlabel <- paste0(dxlabel, " (", pd$unit, ")")
          dylabel <- paste0(dylabel, " (", pd$unit, ")")
        }
        v$ppx$setState(
          list(
            x = unlist(pd$cs.x.raw.m), y = unlist(pd$cs.x.sd),
            xlab = xxlabel, ylab = xylabel
          )
        )

        v$ppy$setState(
          list(
            x = unlist(pd$cs.y.raw.m), y = unlist(pd$cs.y.sd),
            xlab = yxlabel, ylab = yylabel
          )
        )

        v$dp.b.c$setState(
          list(
            x = unlist(pd$cs.xy.raw.m), y = unlist(pd$cs.b),
            xlab = dxlabel,
            ylab = dylabel
          )
        )
        v$c$setContent(c(
          paste0(
            "If the standard deviations seem to be proportional to the measurment procedure X/Y, ",
            "it is an indication that $ln()$ should be used. "
          )
        ))
        rcd <- rm.cv(pd)

        rdd <- rmcs.d(pd, rcd)
        note <- self$results$cvrm$items$note
        note$setContent(
          c(
            tprintf("For each reference material, there are $p=%d$ positions with $k=%d$ replicates in each position.", pd$p, pd$k),
            tprintf("Overall mean value of each reference material: $Mean_{Tot}$"),
            tprintf("Standard deviation of the position means: $s_{Pos-mean}$"),
            tprintf("Pooled standard deviation of the standard deviations within positions: $s_e$"),
            tprintf("The F-test statistic to test the hypothesis of no position effects: $F = ks^2_{Pos-mean}/s^2_e$"),
            tprintf("The degrees of freedom used in F-test: $df_1 =p-1=%d, df_2 =(k-1)\\cdot p= %d$", rcd$df1, rcd$df2),
            tprintf("The F-test criteria with $\\alpha=%s$: $F_{\\alpha,df_1,df_2}=%s$", ftrim(pd$alpha), ftrim(rcd$F.c)),
            tprintf("Estimated standard deviation of the position effects: $s_{Pos} = \\sqrt{s^2_{Pos-mean}-s^2_e/k}$"),
            tprintf("$F \\gt F_{\\alpha,df_1,df_2}$ indicate significant position effects, and $s_{Pos}$ is set to 0 if $F<1$.")
          )
        )
        pex <- self$results$cvrm$items$pex
        pey <- self$results$cvrm$items$pey
        for (rmid in pd$rm.id.u) {
          pex$addRow(
            rowKey = rmid,
            values = list(
              rmid = rmid,
              mt = rcd$mean.tot.x[[rmid]],
              spm = rcd$s.pos.mean.x[[rmid]],
              sp = sqrt(ifelse(
                rcd$s.pos.x2[[rmid]] > 0,
                rcd$s.pos.x2[[rmid]], 0
              )),
              se = rcd$s.e.x[[rmid]],
              F = rcd$F.x[[rmid]],
              pe = ifelse(
                rcd$pos.x.effect[[rmid]],
                "Yes", "No"
              )
            )
          )
          pey$addRow(
            rowKey = rmid,
            values = list(
              rmid = rmid,
              mt = rcd$mean.tot.y[[rmid]],
              spm = rcd$s.pos.mean.y[[rmid]],
              sp = sqrt(ifelse(
                rcd$s.pos.y2[[rmid]] > 0,
                rcd$s.pos.y2[[rmid]], 0
              )),
              se = rcd$s.e.y[[rmid]],
              F = rcd$F.y[[rmid]],
              pe = ifelse(
                rcd$pos.y.effect[[rmid]],
                "Yes", "No"
              )
            )
          )
        }
        pec <- self$results$cvrm$items$c
        pec$setContent(
          pe.conclusion(pd$rm.id.u, rcd$pos.x.effect, rcd$pos.y.effect)
        )
        self$results$cmp$setContent(
          c(
            tprintf("Repeatitons: $k = %d$", pd$k),
            tprintf("Number of clinical samples: $n=%d$", pd$n),
            tprintf("$B_i = Y_i - X_i$, ordered according to ascending values of $(Y_i+X_i)/2$"),
            tprintf("Mean of bias: $B_{CS} = \\sum_{i=1}^n{B_i}/ n =%s$", ftrim(rdd$B_CS)),
            tprintf("Pooled standard deviation from replicates for measurement procedure X and Y:"),
            tprintf("$s_X=\\sqrt{\\sum{SD_X^2}}/n=%s,s_Y=\\sqrt{\\sum{SD_Y^2}}/n=%s$", ftrim(pd$cs.sx), ftrim(pd$cs.sy)),
            tprintf(
              "Contribution to differences from $s_X$ and $s_Y$: $s_E=(s_X^2+s_Y^2)/k=%s$",
              ftrim(rdd$s.e2.cs)
            ),
            tprintf(
              "Standard deviation of bias:"
            ),
            tprintf(
              "$s_B=\\sqrt{\\frac{1}{n-1}\\sum_{i=1}^{n}{(B_i-B_{CS})^2}}=%s$",
              ftrim(rdd$s_B)
            ),
            tprintf("Standard deviation of bias (caculated from sequential differences):"),
            tprintf("$s_{MSSD}=\\sqrt{\\frac{1}{2(n-1)}\\sum_{i=1}^{n-1}{(B_{i+1}-B_i)^2}}=%s$", ftrim(rdd$s_MSSD))
          )
        )
        self$results$ttcs$setContent(
          c(
            tprintf("The Q-test statistic to test the hypothesis of no trend: $Q = (s_{MSSD}/s_B)^2 = %s$", ftrim(rdd$Q)),
            tprintf(
              "The standard deviation of normal distribution: $\\sigma = \\sqrt{(n-2)/(n^2-1)} = %s$",
              ftrim(rdd$SD)
            ),
            tprintf(
              "The Q-test criteria with $\\alpha=%s$: $Q_{\\alpha} = \\mu + z_{1-\\alpha} \\cdot \\sigma = 1 + %s \\cdot %s = %s$",
              ftrim(pd$alpha), ftrim(qnorm(1 - pd$alpha)), ftrim(rdd$SD), ftrim(rdd$Q.c)
            ),
            tprintf("Significant trend: %s ($Q %s Q_{\\alpha}$)", ifelse(rdd$trend, "Yes", "No"), ifelse(rdd$Q > rdd$Q.c, "\\gt", "\\lt")),
            tprintf(
              "$s_d = \\sqrt{%s^2 - s_E^2}=\\sqrt{%f} = %f$",
              ifelse(rdd$trend, "s_{MSSD}", "s_B"),
              rdd$s_d2, ifelse(rdd$s_d2 > 0, sqrt(rdd$s_d2), 0)
            )
          )
        )
        self$results$tdcs$setContent(
          c(
            sprintf(texformat(
              "$\\alpha = %f$"
            ), pd$alpha),
            sprintf(texformat(
              "$F=(s_{MSSD}/s_E)^2 = %f$"
            ), rdd$F),
            sprintf(texformat(
              "$df_1 = \\lfloor \\frac{n}{2} \\rfloor = %d$"
            ), rdd$df1),
            sprintf(texformat(
              paste0(
                "$df_2 =\\lfloor \\frac",
                "{(s_x^2/n+s_y^2/n)^2}",
                "{(s_x^2/n)^2/(n-1)+s_y^2/n)^2/(n-1)}",
                "\\rfloor = %d$"
              )
            ), rdd$df2),
            sprintf(texformat(
              "$F_{(\\alpha,df_1,df_2)}=%f$"
            ), rdd$F.c),
            sprintf(
              texformat(
                "$F %s F_{(\\alpha,df_1,df_2)}$: %sSignificant Differences"
              ), ifelse(rdd$F > rdd$F.c, "\\gt", "\\lt"),
              ifelse(rdd$xy.diff, "", "No ")
            )
          )
        )
        self$results$rma$setContent(
          c(
            sprintf(texformat(
              "$\\alpha = %f$"
            ), pd$alpha),
            sprintf(texformat(
              "$df_1 = p - 1 = %d$"
            ), rcd$df1),
            sprintf(texformat(
              "$df_2 = (k - 1) \\cdot p = (%d - 1) \\cdot %d = %d$"
            ), pd$k, pd$rm.pos.n, rcd$df2),
            sprintf(texformat(
              "$F_{\\alpha,df_1,df_2}=%f$"
            ), rcd$F.c)
          )
        )

        rmc <- rm.c(pd, rcd, rdd)
        self$results$sec$setContent(
          c(
            "SD for random error from measurements of CSs:",
            sprintf(
              texformat(" $s_{e(CS)}=(s_x^2+s_y^2)/k_{cs}=%f$"),
              sqrt(rdd$s.e2.cs)
            ),
            "SD for random error from measurements of RMs:",
            sprintf(
              texformat(
                " $s_{e(RM)}=({\\overline s_{e(x)}}^2+{\\overline s_{e(y)}}^2)/k_{RM}=%f$"
              ),
              sqrt(rcd$pooled.s.e2)
            ),
            "SD for random error from position effects for RMs:",
            sprintf(texformat(
              " $s_{Pos(RM)} = \\sqrt{s_{Pos(x)}^2+s_{Pos(y)}^2}=%f$"
            ), sqrt(rcd$pooled.s.pos.x2 + rcd$pooled.s.pos.y2)),
            "SD for sample specific differences corrected for position effects:",
            sprintf(texformat(
              " $s_{d(corr)}=\\sqrt{s_d^2-s_{Pos(RM)}^2}=%f$"
            ), rdd$s_d_corr)
          )
        )
        self$results$seft$setContent(
          c(
            sprintf(texformat(
              "$F = %s = %f$"
            ), ifelse(rdd$s.e2.cs > rdd$s.e2.rm,
              "s_{e(CS)}^2/s_{e(RM)}^2",
              "s_{e(RM)}^2/s_{e(CS)}^2"
            ), rdd$F.csrm),
            sprintf(texformat(
              "$df_{CS}=n_{CS} \\cdot (k_{CS}-1) = %d$"
            ), rdd$df.cs),
            sprintf(texformat(
              "$df_{RM}=n_{RM} \\cdot p \\cdot (k_{RM}-1) = %d$"
            ), rdd$df.rm),
            sprintf(texformat(
              "$F_{(\\alpha,%s)} = %f$"
            ), ifelse(rdd$s.e2.cs > rdd$s.e2.rm,
              "df_{CS},df_{RM}", "df_{RM},df_{CS}"
            ), rdd$F.c.csrm),
            sprintf(
              texformat(
                "$F %s F_{\\alpha}$ : $s_{e(CS)}$ is%s same with $s_{e(RM)}$"
              ), ifelse(rdd$F.csrm > rdd$F.c.csrm, "\\gt", "\\lt"),
              ifelse(rdd$s.diff.csrm, " not", "")
            )
          )
        )
        self$results$adj$items$r$setContent(
          c(
            sprintf(
              texformat(
                "Adjust Regression Line: $b = %f x %s %f$"
              ), rdd$slope,
              ifelse(rdd$intercept > 0, "+", "-"),
              abs(rdd$intercept)
            ),
            sprintf(
              texformat(
                "Pearson's Correlation: %f"
              ), rdd$corr
            )
          )
        )
        for (rmid in pd$rm.id.u) {
          self$results$adj$items$adj$addRow(
            rowKey = rmid,
            values = list(
              rmid = rmid,
              x = rdd$rm.v.x[[rmid]],
              y = rdd$rm.v.y[[rmid]],
              yl = rmc$rm.y.l[[rmid]],
              yu = rmc$rm.y.u[[rmid]],
              ay = rdd$rm.v.y.adjust[[rmid]],
              ayl = rmc$rm.y.l.adjust[[rmid]],
              ayu = rmc$rm.y.u.adjust[[rmid]]
            )
          )
        }
        self$results$adj$items$udrm$setContent(
          c(
            sprintf(texformat(
              "$C = %.3f$"
            ), pd$C),
            sprintf(texformat(
              "Cov.factor: $C_f = %.2f$"
            ), pd$e.f),
            sprintf(texformat(
              "$s_{Pos-mean(x)}=\\overline s_{e(x)} = %f$"
            ), sqrt(rcd$pooled.s.posmean.x2)),
            sprintf(texformat(
              "$s_{Pos-mean(y)}=\\overline s_{e(y)} = %f$"
            ), sqrt(rcd$pooled.s.posmean.y2)),
            sprintf(texformat(
              "$u(B_{RM})=\\sqrt{(s_{Pos-mean(x)}^2+s_{Pos-mean(y)}^2)/p}=%f$"
            ), rmc$u.B.RM),
            sprintf(texformat(
              "$u(B_{CS})=s_B/\\sqrt{n} = %f$"
            ), rmc$u.B.CS),
            sprintf(texformat(
              "$u(d_{RM})=\\sqrt{u(B_{RM})^2 + u(B_{CS})^2}=%f$"
            ), rmc$u.d.RM),
            sprintf(texformat(
              "$U(d_{RM})=C_f \\cdot u(d_{RM}) = %f$"
            ), rmc$U.d.RM)
          )
        )
        for (rmid in pd$rm.id.u) {
          self$results$ct$addRow(
            rowKey = rmid,
            values = list(
              rmid = rmid,
              ym = rdd$rm.v.y.adjust[[rmid]],
              yl = rmc$rm.y.l.adjust[[rmid]],
              yu = rmc$rm.y.u.adjust[[rmid]],
              cl = -abs(pd$C),
              cu = abs(pd$C),
              c = rmc$commutable[[rmid]]
            )
          )
        }
        ci.list <- list(
          cs.x = unlist(rdd$cs.v.x),
          cs.y = unlist(rdd$cs.v.y.adjust),
          rm.x = unlist(rdd$rm.v.x),
          rm.y = unlist(rdd$rm.v.y.adjust),
          C = pd$C,
          bcs = 0,
          rm.l = rmc$rm.y.l.adjust,
          rm.h = rmc$rm.y.u.adjust,
          rm.id = pd$rm.id.u
        )
        self$results$ci$setState(ci.list)
      }
    )
  )
}
