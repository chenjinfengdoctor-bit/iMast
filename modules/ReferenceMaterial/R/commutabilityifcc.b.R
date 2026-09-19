# This file is a generated template, your changes will not be overwritten
library(ggplot2)
library(outliers)

CommutabilityIFCCClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "CommutabilityIFCCClass",
    inherit = CommutabilityIFCCBase,
    private = list(
      .pp.mp.render = function(image, ...) {
        if (is.null(image$state$x)) {
          return(FALSE)
        }
        x <- image$state$x
        y <- image$state$y
        xlab <- image$state$xlab
        ylab <- image$state$ylab
        if (self$options$logscale) {
          plot <- plot(
            x, y,
            log = "x", pch = 18,
            col = "black", bg = "black",
            xlab = xlab,
            ylab = ylab
          )
        } else {
          plot <- plot(
            x, y,
            pch = 18,
            col = "black", bg = "black",
            xlab = xlab,
            ylab = ylab
          )
        }

        plot <- plot + box(col = "black") + theme(
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
        )
        print(plot)
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
            x = "Mean conc both (x+y)/2",
            y = "Bias"
          )
        print(plot)
        TRUE
      },
      .run = function() {
        welch_df <- function(s1, n1, s2, n2) {
          num <- (s1^2 / n1 + s2^2 / n2)^2
          denom <- ((s1^2 / n1)^2) / (n1 - 1) + ((s2^2 / n2)^2) / (n2 - 1)
          df <- as.integer(num / denom)
          return(df)
        }
        texformat <- function(str) {
          gsub("([{}])", "\\1", str)
        }
        prepare <- function(data, options, self) {
          g.alpha <- options$g.alpha
          rm.pos <- na.omit(data[, options$rmp])
          log.scale <- options$logscale
          C <- abs(options$C / 100)
          rm.pos.u <- unique(rm.pos)
          rm.pos.n <- length(rm.pos.u)
          rmx <- sapply(options$rmxy, function(pair) pair[[1]])
          rmy <- sapply(options$rmxy, function(pair) pair[[2]])
          rm.id <- na.omit(data[, options$rmid])
          rm.id.i <- which(!is.na(data[, options$rmid]))
          rm.id.u <- unique(rm.id)
          rm.outliers <- list()
          rm.x.raw <- data[rm.id.i, rmx]
          rm.y.raw <- data[rm.id.i, rmy]
          for (i in rm.id.i) {
            row.x <- rm.x.raw[i, ]
            row.y <- rm.y.raw[i, ]
            g <- grubbs(row.x, row.y, g.alpha)
            rm.x.raw[i, ] <- g$x
            rm.y.raw[i, ] <- g$y
            if (!is.null(g$outliers)) {
              rm.outliers[[as.character(rm.id[[i]])]] <-
                list(
                  v = g$outliers,
                  p = as.character(rm.pos[[i]])
                )
            }
          }
          rm.x <- rm.x.raw
          rm.y <- rm.y.raw
          ln <- options$ln
          if (ln) {
            rm.x <- log(rm.x)
            rm.y <- log(rm.y)
          }
          rm.x.raw.g <- split(rm.x.raw, rm.id)
          rm.y.raw.g <- split(rm.y.raw, rm.id)
          rm.x.g <- split(rm.x, rm.id)
          rm.y.g <- split(rm.y, rm.id)
          rm.x.p <- split(rm.x, rm.pos)
          rm.y.p <- split(rm.y, rm.pos)

          rm.x.m <- apply(rm.x, 1, function(row) {
            mean(unlist(row), na.rm = TRUE)
          })
          rm.y.m <- apply(rm.y, 1, function(row) {
            mean(unlist(row), na.rm = TRUE)
          })
          rm.x.sd <- apply(rm.x, 1, function(row) {
            sd(unlist(row), na.rm = TRUE)
          })
          rm.y.sd <- apply(rm.y, 1, function(row) {
            sd(unlist(row), na.rm = TRUE)
          })
          rm.x.g.m <- lapply(rm.x.g, function(df) {
            mean(as.matrix(df), na.rm = TRUE)
          })
          rm.x.p.m <- lapply(rm.x.p, function(df) {
            mean(as.matrix(df), na.rm = TRUE)
          })
          rm.y.p.m <- lapply(rm.y.p, function(df) {
            mean(as.matrix(df), na.rm = TRUE)
          })
          rm.y.g.m <- lapply(rm.y.g, function(df) {
            mean(as.matrix(df), na.rm = TRUE)
          })
          rm.xy.m <- setNames(lapply(rm.id.u, function(id) {
            rm.x.m[[id]] / 2 + rm.y.m[[id]] / 2
          }), rm.id.u)
          rm.xy.raw.m <- setNames(lapply(rm.id.u, function(id) {
            mean(as.matrix(rm.x.raw.g[[id]])) / 2 +
              mean(as.matrix(rm.y.raw.g[[id]])) / 2
          }), rm.id.u)
          cs.id <- na.omit(data[, options$csid])
          cs.id.i <- which(!is.na(data[, options$csid]))
          cs.id.u <- unique(cs.id)
          n <- length(cs.id.u)
          csx <- sapply(options$csxy, function(pair) pair[[1]])
          csy <- sapply(options$csxy, function(pair) pair[[2]])
          cs.outliers <- list()
          cs.x.raw <- data[cs.id.i, csx]
          cs.y.raw <- data[cs.id.i, csy]
          for (i in cs.id.i) {
            row.x <- cs.x.raw[i, ]
            row.y <- cs.y.raw[i, ]
            g <- grubbs(row.x, row.y, g.alpha)
            cs.x.raw[i, ] <- g$x
            cs.y.raw[i, ] <- g$y
            if (!is.null(g$outliers)) {
              cs.outliers[[as.character(cs.id[[i]])]] <- g$outliers
            }
          }
          cs.x <- cs.x.raw
          cs.y <- cs.y.raw
          if (ln) {
            cs.x <- log(cs.x)
            cs.y <- log(cs.y)
          }
          cs.x.g <- split(cs.x, cs.id)
          cs.y.g <- split(cs.y, cs.id)
          cs.x.raw.g <- split(cs.x.raw, cs.id)
          cs.y.raw.g <- split(cs.y.raw, cs.id)
          cs.x.m <- lapply(cs.x.g, function(df) {
            mean(as.matrix(df), na.rm = TRUE)
          })
          cs.y.m <- lapply(cs.y.g, function(df) {
            mean(as.matrix(df), na.rm = TRUE)
          })
          cs.x.sd <- lapply(cs.x.g, function(df) {
            sd(as.matrix(df), na.rm = TRUE)
          })
          cs.y.sd <- lapply(cs.y.g, function(df) {
            sd(as.matrix(df), na.rm = TRUE)
          })
          cs.x.raw.m <- lapply(cs.x.raw.g, function(df) {
            mean(as.matrix(df), na.rm = TRUE)
          })
          cs.y.raw.m <- lapply(cs.y.raw.g, function(df) {
            mean(as.matrix(df), na.rm = TRUE)
          })
          cs.xy.m <- setNames(lapply(cs.id.u, function(id) {
            cs.x.m[[id]] / 2 + cs.y.m[[id]] / 2
          }), cs.id.u)
          cs.xy.raw.m <- setNames(lapply(cs.id.u, function(id) {
            cs.x.raw.m[[id]] / 2 + cs.y.raw.m[[id]] / 2
          }), cs.id.u)

          cs.b <- setNames(lapply(cs.id.u, function(id) {
            cs.y.m[[id]] - cs.x.m[[id]]
          }), cs.id.u)
          cs.sx <- sqrt(sum(unlist(cs.x.sd)^2, na.rm = TRUE) / length(cs.id))
          cs.sy <- sqrt(sum(unlist(cs.y.sd)^2, na.rm = TRUE) / length(cs.id))

          k <- length(options$rmxy)
          cs.k <- length(options$csxy)
          e.f <- options$ef
          return(list(
            alpha = options$alpha,
            g.alpha = g.alpha,
            log.scale = log.scale,
            C = C,
            ln = ln,
            p = rm.pos.n,
            rm.pos = rm.pos,
            rm.pos.n = rm.pos.n,
            rm.pos.u = rm.pos.u,
            rm.id = rm.id,
            rm.id.i = rm.id.i,
            rm.id.u = rm.id.u,
            rm.g = rm.id,
            rm.g.u = rm.id.u,
            rm.x = rm.x,
            rm.y = rm.y,
            rm.outliers = rm.outliers,
            rm.x.g = rm.x.g,
            rm.y.g = rm.y.g,
            rm.x.p = rm.x.p,
            rm.y.p = rm.y.p,
            rm.x.m = rm.x.m,
            rm.y.m = rm.y.m,
            rm.x.sd = rm.x.sd,
            rm.y.sd = rm.y.sd,
            rm.x.g.m = rm.x.g.m,
            rm.y.g.m = rm.y.g.m,
            rm.x.p.m = rm.x.p.m,
            rm.y.p.m = rm.y.p.m,
            rm.xy.m = rm.xy.m,
            rm.x.raw = rm.x.raw,
            rm.y.raw = rm.y.raw,
            k = k,
            cs.k = cs.k,
            cs.id = cs.id,
            cs.id.u = cs.id.u,
            cs.id.i = cs.id.i,
            n = n,
            cs.x = cs.x,
            cs.y = cs.y,
            cs.outliers = cs.outliers,
            cs.x.raw = cs.x.raw,
            cs.y.raw = cs.y.raw,
            cs.x.g = cs.x.g,
            cs.y.g = cs.y.g,
            cs.x.raw.g = cs.x.raw.g,
            cs.y.raw.g = cs.y.raw.g,
            cs.x.raw.m = cs.x.raw.m,
            cs.y.raw.m = cs.y.raw.m,
            cs.x.m = cs.x.m,
            cs.y.m = cs.y.m,
            cs.x.sd = cs.x.sd,
            cs.y.sd = cs.y.sd,
            cs.xy.m = cs.xy.m,
            cs.xy.raw.m = cs.xy.raw.m,
            rm.xy.raw.m = rm.xy.raw.m,
            cs.b = cs.b,
            cs.sx = cs.sx,
            cs.sy = cs.sy,
            e.f = e.f
          ))
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
          cs.xy.m <- na.omit(unlist(pd$cs.xy.m))
          indices <- order(cs.xy.m)
          cs.b <- cs.b[indices]
          cs.xy.m <- cs.xy.m[indices]
          b.u <- lm(cs.b ~ cs.xy.m)
          Bi <- diff(cs.b)
          B_CS <- mean(cs.b)
          s_MSSD <- sqrt(
            1 / 2 / pd$n * sum(Bi^2)
          )
          s_B <- sqrt(1 / (pd$n - 1) * sum((cs.b - B_CS)^2))
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
              cs.xy.m = cs.xy.m,
              b.u = b.u,
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
          B_RM <- setNames(sapply(pd$rm.id.u, function(rmid) {
            rcd$mean.tot.y[[rmid]] - rcd$mean.tot.x[[rmid]]
          }), pd$rm.id.u)
          B_CS <- rdd$B_CS
          uB_CS <- rdd$s_B / sqrt(pd$n)
          uB_RM <- sqrt((rcd$pooled.s.posmean.x2 + rcd$pooled.s.posmean.y2) / pd$p)
          d_RM <- B_RM - B_CS
          q <- pd$n
          b.u.summary <- summary(rdd$b.u)
          b.u.constant <- is.constant(b.u.summary, alpha)

          cs.xy.m <- unlist(pd$cs.xy.m)
          rm.xy.m <- unlist(pd$rm.xy.m)
          cs.rm.cover <- (
            min(cs.xy.m) < min(rm.xy.m) &&
              max(cs.xy.m) > max(rm.xy.m)
          )
          condition <- "D"
          if (b.u.constant && cs.rm.cover) {
            # A
            condition <- "A"


            ud_RM <- sqrt(
              (rcd$pooled.s.posmean.x2 + rcd$pooled.s.posmean.y2) / pd$p
                + rdd$s_B^2 / pd$n
            )
          } else {
            c_indices <- get_cs_indices(
              cs.xy.m,
              rm.xy.m
            )
            q <- length(c_indices)
            if (q < 12) {
              stop(sprintf("B: q = %d < 12 is not enough", q))
            }
            q.b <- unlist(pd$cs.b)[c_indices]
            q.xy <- cs.xy.m[c_indices]
            sub_model <- lm(
              q.b ~ q.xy
            )
            sub.is_constant <- is.constant(summary(sub_model), alpha)
            if (sub.is_constant) {
              condition <- "B"
              ud_RM <- sqrt(
                (rcd$pooled.s.posmean.x2 + rcd$pooled.s.posmean.y2) / pd$p
                  + rdd$s_B^2 / q
              )
            } else {
              rm.m.m <- mean(rm.xy.m)
              cs.xy.l.i <- which(cs.xy.m < rm.m.m)
              cs.xy.r.i <- which(cs.xy.m > rm.m.m)
              cs.xy.l <- cs.xy.m[cs.xy.l.i]
              cs.xy.r <- cs.xy.m[cs.xy.r.i]
              cs.b.l <- pd$cs.b[cs.xy.l.i]
              cs.b.r <- pd$cs.r[cs.xy.r.i]
              q.l <- length(cs.xy.l)
              q.r <- length(cs.xy.r)
              q <- 2 * (min(q.l, q.r))
              if (q < 12) {
                stop(sprintf("C: q = %d < 12 is not enough", q))
              }
              lm.l <- lm(cs.b.l ~ cs.xy.l)
              lm.r <- lm(cs.b.r ~ cs.xy.r)
              if (!is.linear(summary(lm.l, alpha)) ||
                !is.linear(summary(lm.r, alpha))) {
                stop(sprintf("C: no linear in each side"))
              }
              ud_RM <- sqrt(
                (rcd$pooled.s.posmean.x2 + rcd$pooled.s.posmean.y2) / pd$p
                  + rdd$s_MSSD^2 / q
              )
              condition <- "C"
            }
          }
          if (condition == "D") {
            stop("D: The prerequisites for situations A, B, and C are not satisfied")
          }
          U.d.RM <- ud_RM * e.f
          d.rm.lower <- d_RM - U.d.RM
          d.rm.higher <- d_RM + U.d.RM
          C <- pd$C
          commutable <- setNames(
            sapply(pd$rm.id.u, function(rmid) {
              if (abs(d_RM[[rmid]]) + U.d.RM <= C) {
                return("Commutable")
              } else if (abs(d_RM[[rmid]]) - U.d.RM > C) {
                return("Non-commutable")
              } else {
                return("Inconclusive")
              }
            }),
            pd$rm.id.u
          )
          u.B.CS.u.B.RM <- uB_CS / uB_RM
          return(list(
            C = C,
            B.RM = B_RM,
            B.CS = B_CS,
            u.B.CS = uB_CS,
            d.RM = d_RM,
            u.d.RM = ud_RM,
            u.B.RM = uB_RM,
            U.d.RM = U.d.RM,
            d.rm.lower = d.rm.lower,
            d.rm.higher = d.rm.higher,
            s.B = rdd$s_B,
            s.MSSD = rdd$s_MSSD,
            q = q,
            condition = condition,
            commutable = commutable,
            u.B.CS.u.B.RM = u.B.CS.u.B.RM
          ))
        }
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        pd <- prepare(self$data, self$options, self)
        if (length(pd$cs.outliers) > 0) {
          self$results$descriptive$items$cso$setVisible(TRUE)
          for (cso in names(pd$cs.outliers)) {
            self$results$descriptive$items$cso$addRow(
              rowKey = cso,
              values = list(cs.id = cso, v = pd$cs.outliers[[cso]])
            )
          }
        }
        if (length(pd$rm.outliers) > 0) {
          self$results$descriptive$items$rmo$setVisible(TRUE)
          for (rmo in names(pd$rm.outliers)) {
            self$results$descriptive$items$rmo$addRow(
              rowKey = rmo,
              values = list(
                rm.id = rmo,
                pos = pd$rm.outliers[[rmo]]$p,
                v = pd$rm.outliers[[rmo]]$v
              )
            )
          }
        }
        mcrm <- self$results$descriptive$items$mcrm
        for (rm.id in pd$rm.id.u) {
          mcrm$addRow(
            rowKey = rm.id,
            values = list(
              rm.id = rm.id,
              mp.x = pd$rm.x.g.m[[rm.id]],
              mp.y = pd$rm.y.g.m[[rm.id]],
              xy = pd$rm.x.g.m[[rm.id]] / 2 + pd$rm.y.g.m[[rm.id]] / 2
            )
          )
        }
        mccs <- self$results$descriptive$items$mccs
        for (cs.id in pd$cs.id.u) {
          mccs$addRow(
            rowKey = cs.id,
            values = list(
              cs.id = cs.id,
              mp.x.m = pd$cs.x.m[[cs.id]],
              mp.x.sd = pd$cs.x.sd[[cs.id]],
              mp.y.m = pd$cs.y.m[[cs.id]],
              mp.y.sd = pd$cs.y.sd[[cs.id]],
              xy.m = pd$cs.xy.m[[cs.id]],
              b = pd$cs.b[[cs.id]]
            )
          )
        }
        sxsy <- self$results$descriptive$items$sxsy
        sxsy$addRow(
          rowKey = "x",
          values = list(xy = "$x$", n = pd$n, s = pd$cs.sx)
        )
        sxsy$addRow(
          rowKey = "y",
          values = list(xy = "$y$", n = pd$n, s = pd$cs.sy)
        )
        self$results$descriptive$items$pp.mp.x$setState(
          list(
            x = unlist(pd$cs.x.raw.m), y = unlist(pd$cs.x.sd),
            xlab = ifelse(self$options$logscale,
              "Conc x (log scale)", "Conc x"
            ),
            ylab = ifelse(pd$ln, "SD ln(conc)", "SD conc")
          )
        )
        self$results$descriptive$items$pp.mp.y$setState(
          list(
            x = unlist(pd$cs.y.raw.m), y = unlist(pd$cs.y.sd),
            xlab = ifelse(self$options$logscale,
              "Conc y (log scale)", "Conc y"
            ),
            ylab = ifelse(pd$ln, "SD ln(conc)", "SD conc")
          )
        )
        self$results$descriptive$items$dp.b.c$setState(
          list(
            x = unlist(pd$cs.xy.raw.m), y = unlist(pd$cs.b),
            xlab = ifelse(self$options$logscale,
              "Conc Both (x+y)/2 (log scale)", "Conc Both (x+y)/2"
            ),
            ylab = ifelse(pd$ln, "Diff ln(conc)", "Diff conc")
          )
        )

        rcd <- rm.cv(pd)
        rdd <- rmcs.d(pd, rcd)

        self$results$cmp$setContent(
          c(
            sprintf(texformat("Repetitons: $k = %d$"), pd$k),
            sprintf(texformat("Number: $n=%d$"), pd$n),
            sprintf(texformat("Mean of Bias (y-x): $B_{CS} =%f$"), rdd$B_CS),
            sprintf(texformat("$B_i = y_i - x_i$,sorted by $(y_i+x_i)/2$")),
            sprintf(
              texformat("Pooled SD from replicates for method x: $s_x=\\frac{\\sqrt{\\sum{SD_x^2}}}{n}=%f$"),
              pd$cs.sx
            ),
            sprintf(
              texformat("Pooled SD from replicates for method y: $s_y=\\frac{\\sqrt{\\sum{SD_y^2}}}{n}=%f$"),
              pd$cs.sy
            ),
            sprintf(texformat(
              "Contribution to differences from $s_x$ and $s_y$: $s_E=\\frac{s_x^2+s_y^2}{k}=%f$"
            ), sqrt(rdd$s.e2.cs)),
            sprintf(texformat("SD of B: $s_B=\\sqrt{\\frac{1}{n-1}\\sum_{i=1}^{n}{(B_i-B_{CS})^2}}=%f$"), rdd$s_B),
            sprintf(texformat(
              "SD of B caculated from sequential differences:\n $s_{MSSD}=\\sqrt{\\frac{1}{2(n-1)}\\sum_{i=1}^{n-1}{(B_{i+1}-B_i)^2}}=%f$"
            ), rdd$s_MSSD)
          )
        )
        self$results$ttcs$setContent(
          c(
            sprintf(texformat(
              "$\\alpha = %f$"
            ), pd$alpha),
            sprintf(texformat(
              "$Q = (s_{MSSD}/s_B)^2 = %f$"
            ), rdd$Q),
            sprintf(texformat(
              "$Q_{\\alpha} = \\mu + z_{1-\\alpha} \\cdot \\sigma = 1 + %f \\cdot %f = %f$"
            ), qnorm(1 - pd$alpha), rdd$SD, rdd$Q.c),
            sprintf(
              texformat(
                "$Q %s Q_{\\alpha}$: %sSignificant Trend"
              ), ifelse(rdd$Q > rdd$Q.c, "\\gt", "\\lt"),
              ifelse(rdd$trend, "", "No ")
            ),
            sprintf(
              texformat(
                "$s_d = \\sqrt{%s^2 - s_E^2}=\\sqrt{%f} = %f$"
              ), ifelse(rdd$trend, "s_{SMSSD}", "s_B"),
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
                "{(\\frac{s_x^2}{n}+\\frac{s_y^2}{n})^2}",
                "{\\frac{(\\frac{s_x^2}{n})^2}{n-1}+\\frac{(\\frac{s_y^2}{n})^2}{n-1}}",
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
        for (rmid in pd$rm.id.u) {
          self$results$rmxa$addRow(
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
          self$results$rmya$addRow(
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
        condition <- list(
          A = paste(
            "The bias function $b(\\mu)$ is approximately constant",
            "in the whole concentration interval, and the CSs",
            "bracket the concentration of the RM",
            sep = " "
          ),
          B = paste(
            "The bias function $b(\\mu)$ is approximately constant",
            "in a concentration interval enclosing the RM",
            sep = " "
          ),
          C = paste(
            "The bias function $b(\\mu)$ has an approximately",
            "linear trend in an interval where there are $q/2$",
            "CSs on each side of the RM",
            sep = " "
          ),
          D = paste(
            "The prerequisites for situations A, B,",
            "and C are not satisfied (e.g., we have none or only a",
            "few CSs with concentrations close to that of the RM)",
            sep = " "
          )
        )
        u.d.RM.latex <- list(
          A = "\\sqrt{(s_{Pos-mean(x)}^2+s_{Pos-mean(y)}^2)/p+s_B^2/n}",
          B = "\\sqrt{(s_{Pos-mean(x)}^2+s_{Pos-mean(y)}^2)/p+(s_B^2)/q}",
          C = "\\sqrt{(s_{Pos-mean(x)^2+s_{Pos-mean(y)}^2)/p+s_MSSD^2/q}"
        )

        self$results$c$setContent(
          c(
            sprintf(texformat(
              "$C = %f$"
            ), pd$C),
            sprintf(texformat(
              "Cov.factor: $C_f = %f$"
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
              "Condition %s: %s"
            ), rmc$condition, condition[[rmc$condition]]),
            sprintf(texformat(
              "$u(d_{RM})=%s=%f$"
            ), u.d.RM.latex[[rmc$condition]], rmc$u.d.RM),
            sprintf(texformat(
              "$U(d_{RM})=C_f \\cdot u(d_{RM}) = %f$"
            ), rmc$U.d.RM),
            sprintf(texformat(
              "$u(B_{CS})/u_(B_{RM})=%f$"
            ), rmc$u.B.CS.u.B.RM)
          )
        )
        for (rmid in pd$rm.id.u) {
          self$results$ct$addRow(
            rowKey = rmid,
            values = list(
              rmid = rmid,
              BRM = rmc$B.RM[[rmid]],
              dRM = rmc$d.RM[[rmid]],
              ull = rmc$d.rm.lower[[rmid]],
              uhl = rmc$d.rm.higher[[rmid]],
              c = rmc$commutable[[rmid]]
            )
          )
        }
        ci.list <- list(
          cs.x = unlist(pd$cs.xy.raw.m),
          cs.y = unlist(pd$cs.b),
          rm.x = unlist(pd$rm.xy.raw.m),
          rm.y = rmc$B.RM,
          C = pd$C,
          bcs = rmc$B.CS,
          rm.l = rmc$B.RM - rmc$U.d.RM,
          rm.h = rmc$B.RM + rmc$U.d.RM,
          rm.id = pd$rm.id.u
        )
        self$results$ci$setState(ci.list)
      }
    )
  )
}
