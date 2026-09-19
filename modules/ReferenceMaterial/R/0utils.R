library(goftest)
library(ggplot2)
library(scales)
library(lmtest)
library(outliers)
texfmt <- function(str) {
  gsub("([{}])", "\\1", str)
}

solve <- function(a, b, c, only.positive = TRUE, range.in = NULL) {
  d <- b^2 - 4 * a * c
  r1 <- (-b + sqrt(d)) / (2 * a)
  r2 <- (-b - sqrt(d)) / (2 * a)
  if (only.positive) {
    if (r1 < 0) {
      r1 <- NA
    }
    if (r2 < 0) {
      r2 <- NA
    }
  }
  if (!is.null(range.in)) {
    if (!is.na(r1) && (r1 < range.in[1] || r1 > range.in[2])) {
      r1 <- NA
    }
    if (!is.na(r2) && (r2 < range.in[1] || r2 > range.in[2])) {
      r2 <- NA
    }
  }
  if (is.na(r1) && is.na(r2)) {
    return(NULL)
  }
  return(na.omit(c(r1, r2)))
}
custom_breaks <- function(x, lo = 5, expand.ratio = 0.1) {
  r <- max(x) - min(x)
  seq(min(x) - r * expand.ratio, max(x) + r * expand.ratio, length.out = lo)
}
distribution.test <- function(xv, yv, alpha) {
  xu <- ad.test(xv, "punif", min = 0.98 * min(xv), max = 1.02 * max(xv), estimated = FALSE)
  yu <- ad.test(yv, "punif", min = 0.98 * min(yv), max = 1.02 * max(yv), estimated = FALSE)
  xn <- ad.test(xv, "pnorm", mean = mean(xv), sd = sd(xv), estimated = FALSE)
  yn <- ad.test(yv, "pnorm", mean = mean(yv), sd = sd(yv), estimated = FALSE)
  xe <- ad.test(xv, "pexp", rate = 1 / mean(xv), estimated = FALSE)
  ye <- ad.test(yv, "pexp", rate = 1 / mean(yv), estimated = FALSE)
  x.d.pass <- list()
  y.d.pass <- list()
  if (xu$p.value >= alpha) {
    x.d.pass[["uniform"]] <- TRUE
  }
  if (xn$p.value >= alpha) {
    x.d.pass[["normal"]] <- TRUE
  }
  if (xe$p.value >= alpha) {
    x.d.pass[["exponential"]] <- TRUE
  }
  if (yu$p.value >= alpha) {
    y.d.pass[["uniform"]] <- TRUE
  }
  if (yn$p.value >= alpha) {
    y.d.pass[["normal"]] <- TRUE
  }
  if (ye$p.value >= alpha) {
    y.d.pass[["exponential"]] <- TRUE
  }
  return(
    list(
      xu = xu,
      yu = yu,
      xn = xn,
      yn = yn,
      xe = xe,
      ye = ye,
      x.d.pass = x.d.pass,
      y.d.pass = y.d.pass
    )
  )
}
d.conclusion <- function(x.d.names, y.d.names) {
  x.s <- ifelse(length(x.d.names) > 1, "s", "")
  y.s <- ifelse(length(y.d.names) > 1, "s", "")
  if (length(x.d.names) == 0 && length(y.d.names) != 0) {
    return(
      paste0(
        "The clinical sample data for measurement procedure X does not appear to follow any of the tested distributions. ",
        sprintf(
          "However, the data for measurement procedure Y follows the %s distribution%s.",
          join(y.d.names), y.s
        )
      )
    )
  } else if (length(x.d.names) != 0 && length(y.d.names) == 0) {
    return(paste0(
      sprintf(
        "The clinical sample data for measurement procedure X follows the %s distribution%s. ",
        join(x.d.names), x.s
      ),
      sprintf(
        "However, the data for measurement procedure Y does not appear to follow any of the tested distributions."
      )
    ))
  } else if (length(x.d.names) == 0 && length(y.d.names) == 0) {
    return(
      "All clinical sample data for measurement procedures X and Y do not appear to follow any of the tested distributions."
    )
  } else if (setequal(x.d.names, y.d.names)) {
    return(
      sprintf(
        "All clinical sample data for measurement procedures X and Y follow the %s distribution%s.",
        join(x.d.names), x.s
      )
    )
  } else {
    return(paste0(
      sprintf(
        "The clinical sample data for measurement procedure X follows the %s distribution%s. ",
        join(x.d.names), x.s
      ),
      sprintf(
        "However, the data for measurement procedure Y follows the %s distribution%s.",
        join(y.d.names), y.s
      )
    ))
  }
}
ep14outliers <- function(pd) {
  cs.id.outliers <- list()
  cs.id.remove <- list()
  n.outliers <- 0
  nh <- pd$cs.k
  n <- length(pd$cs.x.m)
  q <- qtukey(p = 0.99, nmeans = nh, df = length(pd$cs.x.m) * (nh - 1))
  cs.x.r <- list()
  cs.y.r <- list()
  pd$cs.x.e.sd <- cs.x.e.sd <- sqrt(1 / n * sum(unlist(pd$cs.x.sd)^2))
  pd$cs.y.e.sd <- cs.y.e.sd <- sqrt(1 / n * sum(unlist(pd$cs.y.sd)^2))
  x.qsd <- q * cs.x.e.sd
  y.qsd <- q * cs.y.e.sd
  for (csid in pd$cs.id.u) {
    cs.x.r[[csid]] <- x.r <- max(pd$cs.x.g[[csid]]) - min(pd$cs.x.g[[csid]])
    cs.y.r[[csid]] <- y.r <- max(pd$cs.y.g[[csid]]) - min(pd$cs.y.g[[csid]])
    if ((x.r > x.qsd) || (y.r > y.qsd)) {
      cs.id.outliers[[as.character(csid)]] <- TRUE
      cs.id.remove[[as.character(csid)]] <- TRUE
      n.outliers <- n.outliers + 1
    } else {
      cs.id.outliers[[as.character(csid)]] <- FALSE
    }
  }
  cs.id.removed <- names(cs.id.remove)
  pd$cs.x.m <- pd$cs.x.m[!names(pd$cs.x.m) %in% cs.id.removed]
  pd$cs.y.m <- pd$cs.y.m[!names(pd$cs.y.m) %in% cs.id.removed]
  pd$cs.x.sd <- pd$cs.x.sd[!names(pd$cs.x.sd) %in% cs.id.removed]
  pd$cs.y.sd <- pd$cs.y.sd[!names(pd$cs.y.sd) %in% cs.id.removed]
  pd$cs.id.raw <- pd$cs.id.u
  pd$cs.id.u <- setdiff(pd$cs.id.u, cs.id.removed)
  pd$cs.id.removed <- cs.id.removed
  pd$cs.id.outliers <- cs.id.outliers
  pd$qcv <- q
  pd$cs.x.r <- cs.x.r
  pd$cs.y.r <- cs.y.r
  pd$cs.x.qsd <- x.qsd
  pd$cs.y.qsd <- y.qsd
  pd$n.outliers <- n.outliers
  return(pd)
}
ep14deming <- function(pd) {
  nh <- pd$cs.k
  x.m <- unlist(pd$cs.x.m)
  y.m <- unlist(pd$cs.y.m)
  x.sd <- unlist(pd$cs.x.sd)
  y.sd <- unlist(pd$cs.y.sd)
  n <- length(x.m)
  npc <- pd$k
  x.m.m <- mean(x.m)
  y.m.m <- mean(y.m)
  x.m.s2 <- sum((x.m - x.m.m)^2) / n
  y.m.s2 <- sum((y.m - y.m.m)^2) / n
  xy.m.s <- sum((x.m - x.m.m) * (y.m - y.m.m)) / n
  x.e.s2 <- 1 / n * sum(x.sd^2)
  y.e.s2 <- 1 / n * sum(y.sd^2)
  lambda <- y.e.s2 / x.e.s2
  bh <- (y.m.s2 - lambda * x.m.s2 + sqrt(
    ((y.m.s2 - lambda * x.m.s2)^2 + 4 * lambda * xy.m.s^2)
  )) /
    (2 * xy.m.s)
  ah <- y.m.m - bh * x.m.m
  bh.e.s2 <- bh^2 / n / xy.m.s^2 * (x.m.s2 * y.m.s2 - xy.m.s^2)
  t <- qt(1 - pd$alpha / 2, n * (nh - 1))
  pi <- function(x.m.pc) {
    y.pc.pred <- ah + bh * x.m.pc
    y.s.pc.pred <- sqrt(
      (x.m.pc - x.m.m)^2 * bh.e.s2 +
        (bh^2 * x.e.s2 + y.e.s2) * (1 + 1 / n) / npc
    )
    return(
      c(
        y.pc.pred - t * y.s.pc.pred,
        y.pc.pred + t * y.s.pc.pred
      )
    )
  }
  pi.x <- seq(min(x.m), max(x.m), length.out = 20)
  pi.b <- t(sapply(pi.x, pi))
  pi.rm <- lapply(pd$rm.x.g.m, pi)
  rm.c <- setNames(sapply(pd$rm.g.u, function(rmid) {
    y <- pd$rm.y.g.m[[rmid]]
    return(ifelse(
      y <= pi.rm[[rmid]][[2]] && y >= pi.rm[[rmid]][[1]],
      "Commutable",
      "Non-commutable"
    ))
  }), pd$rm.g.u)
  return(list(
    x.m = x.m,
    y.m = y.m,
    n = n,
    nh = nh,
    x.m.m = x.m.m,
    y.m.m = y.m.m,
    x.m.s2 = x.m.s2,
    y.m.s2 = y.m.s2,
    xy.m.s = xy.m.s,
    x.e.s2 = x.e.s2,
    y.e.s2 = y.e.s2,
    lambda = lambda,
    bh = bh,
    ah = ah,
    t = t,
    bh.e.s2 = bh.e.s2,
    pi.x = pi.x,
    pi.b = pi.b,
    pi.rm = pi.rm,
    rm.c = rm.c
  ))
}
ep30deming <- function(pd, odc, lil, uil) {
  nh <- pd$cs.k
  x.m <- unlist(pd$cs.x.m)
  y.m <- unlist(pd$cs.y.m)
  x.sd <- unlist(pd$cs.x.sd)
  y.sd <- unlist(pd$cs.y.sd)
  n <- length(x.m)
  x.m.m <- mean(x.m)
  y.m.m <- mean(y.m)
  x.m.s2 <- sum((x.m - x.m.m)^2) / n
  y.m.s2 <- sum((y.m - y.m.m)^2) / n
  xy.m.s <- sum((x.m - x.m.m) * (y.m - y.m.m)) / n
  x.e.s2 <- 1 / n * sum(x.sd^2)
  y.e.s2 <- 1 / n * sum(y.sd^2)
  lambda <- y.e.s2 / x.e.s2
  bh <- (y.m.s2 - lambda * x.m.s2 + sqrt(
    ((y.m.s2 - lambda * x.m.s2)^2 + 4 * lambda * xy.m.s^2)
  )) /
    (2 * xy.m.s)
  ah <- y.m.m - bh * x.m.m
  bh.e.s2 <- bh^2 / n / xy.m.s^2 * (x.m.s2 * y.m.s2 - xy.m.s^2)
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
    x.m = x.m,
    y.m = y.m,
    n = n,
    nh = nh,
    x.m.m = x.m.m,
    y.m.m = y.m.m,
    x.m.s2 = x.m.s2,
    y.m.s2 = y.m.s2,
    xy.m.s = xy.m.s,
    x.e.s2 = x.e.s2,
    y.e.s2 = y.e.s2,
    lambda = lambda,
    bh = bh,
    ah = ah,
    bh.e.s2 = bh.e.s2,
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
join <- function(items) {
  l <- length(items)
  if (l == 0) {
    return("")
  } else if (l <= 2) {
    return(paste(items, collapse = " and "))
  } else {
    return(paste0(paste(items[1:(l - 1)], collapse = ", "), ", and ", items[l]))
  }
}
ols.render <- function(image) {
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

  df <- data.frame(x = image$state$x, y = image$state$y)
  eq_label <- sprintf(
    "Y == %.2f*italic(x)%s%.2f",
    image$state$k,
    ifelse(image$state$b >= 0, "+ ", "- "),
    abs(image$state$b)
  )
  x_min <- min(range(df$x))
  y_mid <- mean(range(df$y))
  df$group <- "Clinical samples"

  plot <- ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(color = "Clinical samples"), shape = 18, size = 5) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      aes(color = "Linear (clinical samples)"),
      linetype = "solid",
      fullrange = FALSE,
      size = 0.5
    ) +
    annotate("text",
      x = x_min, y = y_mid, label = eq_label, parse = TRUE,
      hjust = -0.1, vjust = -2, size = 5, family = "Times New Roman"
    ) +
    scale_color_manual(
      name = "", values = c("Linear (clinical samples)" = "black", "Clinical samples" = "#4F81BD")
    ) +
    labs(
      x = xlabel,
      y = ylabel
    ) +
    scale_x_continuous(
      breaks = custom_breaks(df$x),
      limits = custom_breaks(df$x, 2),
      labels = label_number(accuracy = 0.01),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = custom_breaks(df$y),
      limits = custom_breaks(df$y, 2),
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
bias.prepare.data <- function(data, options) {
  g.alpha <- options$g.alpha
  rm.pos <- na.omit(data[, options$rmp])


  log.scale <- options$logscale
  C <- abs(options$C / 100)
  rm.pos.u <- unique(rm.pos)
  rm.pos.n <- length(rm.pos.u)
  # rmx <- sapply(options$rmxy, function(pair) pair[[1]])
  # rmy <- sapply(options$rmxy, function(pair) pair[[2]])
  rmx <- options$rmx
  rmy <- options$rmy
  rm.id <- na.omit(data[, options$rmid])
  rm.id.i <- which(!is.na(data[, options$rmid]))
  rm.id.u <- unique(rm.id)
  rm.outliers <- list()
  rm.x.raw <- data[rm.id.i, rmx]
  rm.y.raw <- data[rm.id.i, rmy]
  rm.g.n <- 0
  for (i in rm.id.i) {
    row.x <- rm.x.raw[i, ]
    row.y <- rm.y.raw[i, ]
    g <- grubbs(row.x, row.y, g.alpha)
    rm.g.n <- g$n
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
  mt <- options$mt
  unit <- options$unit
  if (mt != "") {
    mt <- paste0(mt, ", ")
  }
  if (ln) {
    unit <- "ln"
    rm.x <- log(rm.x)
    rm.y <- log(rm.y)
  }
  unit <- paste0(mt, unit)
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
  rm.x.g.sd <- lapply(rm.x.g, function(df) {
    sd(as.matrix(df), na.rm = TRUE)
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
  rm.y.g.sd <- lapply(rm.y.g, function(df) {
    sd(as.matrix(df), na.rm = TRUE)
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
  # csx <- sapply(options$csxy, function(pair) pair[[1]])
  csx <- options$csx
  # csy <- sapply(options$csxy, function(pair) pair[[2]])
  csy <- options$csy
  cs.outliers <- list()
  cs.x.raw <- data[cs.id.i, csx]
  cs.y.raw <- data[cs.id.i, csy]
  cs.g.n <- 0
  for (i in cs.id.i) {
    row.x <- cs.x.raw[i, ]
    row.y <- cs.y.raw[i, ]
    g <- grubbs(row.x, row.y, g.alpha)
    cs.g.n <- g$n
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

  k <- length(options$rmx)
  cs.k <- length(options$csx)
  e.f <- options$ef
  return(list(
    alpha = options$alpha,
    g.alpha = g.alpha,
    log.scale = log.scale,
    rm.g.n = rm.g.n,
    cs.g.n = cs.g.n,
    C = C,
    ln = ln,
    unit = unit,
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
    rm.x.g.sd = rm.x.g.sd,
    rm.y.g.m = rm.y.g.m,
    rm.y.g.sd = rm.y.g.sd,
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
regression.prepare.data <- function(data, options) {
  ln <- options$ln
  mt <- options$mt
  unit <- options$unit
  if (mt != "") {
    mt <- paste0(mt, ", ")
  }
  if (ln) {
    unit <- "lg"
  }
  unit <- paste0(mt, unit)
  rm.pos <- na.omit(data[, options$rmp])
  rm.pos.u <- unique(rm.pos)
  rm.pos.n <- length(rm.pos.u)
  # rmx <- sapply(options$rmxy, function(pair) pair[[1]])
  # rmy <- sapply(options$rmxy, function(pair) pair[[2]])
  rmx <- options$rmx
  rmy <- options$rmy
  rm.id <- na.omit(data[, options$rmid])
  rm.id.i <- which(!is.na(data[, options$rmid]))
  rm.id.u <- unique(rm.id)
  rm.x.raw <- data[rm.id.i, rmx]
  rm.y.raw <- data[rm.id.i, rmy]
  rm.x <- rm.x.raw
  rm.y <- rm.y.raw
  if (ln) {
    rm.x <- log10(rm.x)
    rm.y <- log10(rm.y)
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
  rm.x.g.sd <- lapply(rm.x.g, function(df) {
    sd(as.matrix(df), na.rm = TRUE)
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
  rm.y.g.sd <- lapply(rm.y.g, function(df) {
    sd(as.matrix(df), na.rm = TRUE)
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
  # csx <- sapply(options$csxy, function(pair) pair[[1]])
  # csy <- sapply(options$csxy, function(pair) pair[[2]])
  csx <- options$csx
  csy <- options$csy
  cs.x.raw <- data[cs.id.i, csx]
  cs.y.raw <- data[cs.id.i, csy]
  cs.x <- cs.x.raw
  cs.y <- cs.y.raw
  if (ln) {
    cs.x <- log10(cs.x)
    cs.y <- log10(cs.y)
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

  k <- length(options$rmx)
  cs.k <- length(options$csx)
  return(list(
    alpha = options$alpha,
    # g.alpha = g.alpha,
    # C = C,
    unit = unit,
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
    rm.x.g.sd = rm.x.g.sd,
    rm.y.g.sd = rm.y.g.sd,
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
    cs.sy = cs.sy
    # e.f = e.f
  ))
}
visualization.data <- function(uxm, uym) {
  cs.ols.lm <- lm(uym ~ uxm)
  cs.cor <- cor(uxm, uym, method = "pearson")
  cs.cor.test <- cor.test(uxm, uym, method = "pearson")
  k <- coef(cs.ols.lm)[2]
  b <- coef(cs.ols.lm)[1]
  level <- if (abs(cs.cor) < 0.2) {
    "Very weak"
  } else if (abs(cs.cor) < 0.4) {
    "Weak"
  } else if (abs(cs.cor) < 0.6) {
    "Moderate"
  } else if (abs(cs.cor) < 0.8) {
    "Strong"
  } else {
    "Very strong"
  }
  conclusion <- c(
    sprintf(
      texfmt("Regression equations: $\\text{Y} = %.2fx %s %.2f$"),
      k, ifelse(b > 0, "+", "-"), abs(b)
    ),
    sprintf("Correlation method: %s", cs.cor.test$method),
    sprintf(
      texfmt("$R^2 = %.3f$, $r = %.3f$ and $P = %.3f$"),
      summary(cs.ols.lm)$r.squared,
      cs.cor, cs.cor.test$p.value
    ),
    sprintf(
      "Correlation strength: %s %s correlation",
      level, ifelse(cs.cor > 0, "positive", "negative")
    )
  )
  return(
    list(
      k = k,
      b = b,
      conclusion = conclusion
    )
  )
}
d.render <- function(image) {
  if (is.null(image$state)) {
    return(FALSE)
  }
  xlabel <- "Average of measurement procedure X and Y"
  ylabel <- "Measurement procedure X minus procedure Y"
  unit <- image$state$unit
  if (unit != "") {
    xlabel <- paste0(xlabel, " (", unit, ")")
    ylabel <- paste0(ylabel, " (", unit, ")")
  }
  df <- data.frame(x = image$state$x, y = image$state$y)
  plot <- ggplot(df, aes(x = x, y = y)) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") + # dashed
    annotate("text",
      x = max(df$x) + 0.5, y = 0, label = "0.00",
      hjust = 0, vjust = -0.5, size = 5,
      family = "Times New Roman"
    ) +
    geom_point(color = "#4F81BD", shape = 18, size = 5) +
    # expand_limits(x = 0, y = 0) +
    labs(
      x = xlabel,
      y = ylabel
    ) +
    scale_x_continuous(
      breaks = custom_breaks(df$x),
      limits = custom_breaks(df$x, 2),
      labels = label_number(accuracy = 0.01),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = custom_breaks(df$y),
      limits = custom_breaks(df$y, 2),
      labels = label_number(accuracy = 0.01),
      expand = c(0, 0)
    ) +
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
res.deming.normal <- function(bh, ah, uxm, uym, alpha) {
  res <- (bh * uxm - uym + ah) / sqrt(bh^2 + 1)
  rnt <- shapiro.test(res)
  conclusion <- c(
    sprintf(texfmt(
      "$\\hat \\beta_H = %f$"
    ), bh),
    sprintf(texfmt(
      "$\\hat \\alpha_H = %f$"
    ), ah),
    sprintf(texfmt(
      "Orthogonal residual: $(\\hat \\beta_H \\overline X_i -\\overline Y_i + \\hat \\alpha_H)/\\sqrt{\\hat \\beta_H^2 + 1}$"
    )),
    sprintf("Method: %s", rnt$method),
    sprintf(
      "Value of the Shapiro-Wilk statistic $W$ = %f",
      rnt$statistic
    ),
    sprintf(
      "$P$ = %f and $\\alpha$ = %.3f, $P$ %s $\\alpha$",
      rnt$p.value, alpha,
      ifelse(rnt$p.value < alpha, "<", ">")
    ),
    sprintf(
      "Passed residual normality test: %s",
      ifelse(rnt$p.value < alpha, "No", "Yes")
    )
  )
  return(list(
    conclusion = conclusion,
    p.value = rnt$p.value
  ))
}
res.ols.normal <- function(bh, ah, uxm, uym, alpha) {
  res <- bh * uxm - uym + ah
  rnt <- shapiro.test(res)
  return(list(
    p.value = rnt$p.value,
    conclusion = c(
      sprintf(texfmt(
        "$\\hat \\beta_H = %f$"
      ), bh),
      sprintf(texfmt(
        "$\\hat \\alpha_H = %f$"
      ), ah),
      sprintf(texfmt(
        "Residual: $\\hat \\beta_H \\overline X_i -\\overline Y_i + \\hat \\alpha_H$"
      )),
      sprintf("Method: %s", rnt$method),
      sprintf(
        "Value of the Shapiro-Wilk statistic $W$ = %f",
        rnt$statistic
      ),
      sprintf(
        "$P$ = %f and $\\alpha$ = %.3f, $P$ %s $\\alpha$",
        rnt$p.value,
        alpha,
        ifelse(rnt$p.value < alpha, "<", ">")
      ),
      sprintf(
        "Passed residual normality test: %s",
        ifelse(rnt$p.value < alpha, "No", "Yes")
      )
    )
  ))
}
rvh.test <- function(bh, ah, uxm, uym, alpha) {
  bp <- bptest(uym ~ uxm)
  return(
    list(
      p.value = bp$p.value,
      conclusion = c(
        sprintf(texfmt(
          "$\\hat \\beta_H = %f$"
        ), bh),
        sprintf(texfmt(
          "$\\hat \\alpha_H = %f$"
        ), ah),
        sprintf(texfmt(
          "Residual: $\\hat \\beta_H \\overline X_i -\\overline Y_i + \\hat \\alpha_H$"
        )),
        sprintf("Method: %s", bp$method),
        sprintf("Degrees of freedom: $v$ = %d", bp$parameter),
        sprintf(
          "$P$ = %f and $\\alpha$ = %.3f, $P$ %s $\\alpha$",
          bp$p.value,
          alpha,
          ifelse(bp$p.value < alpha, "<", ">")
        ),
        sprintf(
          "Passed homoscedasticity test: %s",
          ifelse(bp$p.value < alpha, "No", "Yes")
        )
      )
    )
  )
}
c.conclusion <- function(c.result) {
  df <- data.frame(id = names(c.result), value = c.result, stringsAsFactors = FALSE)
  grouped <- split(df$id, df$value)
  sentences <- sapply(names(grouped), function(val) {
    ids <- grouped[[val]]
    verb <- ifelse(length(ids) > 1, "are", "is")
    return(sprintf(
      "Reference material %s %s %s.",
      join(ids), verb, gsub("<br>", " ", val)
    ))
  })
  return(sentences)
}
c.summary <- function(d.rnt, o.rnt, o.rvh, small.interval) {
  s <- c(
    "Based on the results of the analysis of the three regressions described above, ",
    "together with the hypothesis testing of the data, "
  )
  if (o.rnt && o.rvh && !d.rnt) {
    s <- c(s, "the results of the ordinary least squares regression and Passing-Bablok regression were recommended.")
  } else if (o.rnt && o.rvh && d.rnt && !small.interval) {
    s <- c(s, "the results of the Deming regression, ordinary least squares regression, and Passing-Bablok regression were recommended.")
  } else if (d.rnt && !(o.rnt && o.rvh) && !small.interval) {
    s <- c(s, "the results of the Deming regression and Passing-Bablok regression were recommended.")
  } else {
    s <- c(s, "the results of the Passing-Bablok regression were recommended.")
  }
  s <- c(s, "\n")
  if (!(o.rnt && o.rvh)) {
    s <- c(s, "The results of the ordinary least squares regression can not be recommended because it did not pass the residual normality or/and homoscedasticity tests. ")
  }
  if ((!d.rnt) || small.interval) {
    s <- c(s, "The results of the Deming regression were not recommended")
    if (small.interval) {
      s <- c(s, " because it did not pass the residual normality test")
    }
    s <- c(s, ".")
  }
  return(paste0(s, collapse = ""))
}
tprintf <- function(fmt, ...) {
  sprintf(texfmt(fmt), ...)
}
ftrim <- function(v) {
  r <- format(v, scientific = FALSE, trim = TRUE)
  r <- sub("(\\.[0-9]*[1-9])0+$", "\\1", r)
  r <- sub("\\.0+$", "", r)
  if (r == "") {
    r <- "0"
  }
  return(r)
}
grubbs.test <- function(x, type = 10, opposite = FALSE, two.sided = FALSE) {
  if (sum(c(10, 11, 20) == type) == 0) {
    stop("Incorrect type")
  }
  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if (type == 11) {
    g <- (x[n] - x[1]) / sd(x)
    u <- var(x[2:(n - 1)]) / var(x) * (n - 3) / (n - 1)
    pval <- 1 - pgrubbs(g, n, type = 11)
    method <- "Grubbs test for two opposite outliers"
    alt <- paste(x[1], "and", x[n], "are outliers")
    alt_val <- c(x[1], x[n])
  } else if (type == 10) {
    if (xor(((x[n] - mean(x)) < (mean(x) - x[1])), opposite)) {
      alt <- paste("lowest value", x[1], "is an outlier")
      alt_val <- c(x[1])
      o <- x[1]
      d <- x[2:n]
    } else {
      alt <- paste("highest value", x[n], "is an outlier")
      alt_val <- c(x[n])
      o <- x[n]
      d <- x[1:(n - 1)]
    }
    g <- abs(o - mean(x)) / sd(x)
    u <- var(d) / var(x) * (n - 2) / (n - 1)
    pval <- 1 - pgrubbs(g, n, type = 10)
    method <- "Grubbs test for one outlier"
  } else {
    if (xor(((x[n] - mean(x)) < (mean(x) - x[1])), opposite)) {
      alt <- paste("lowest values", x[1], ",", x[2], "are outliers")
      alt_val <- c(x[1], x[2])
      u <- var(x[3:n]) / var(x) * (n - 3) / (n - 1)
    } else {
      alt <- paste(
        "highest values", x[n - 1], ",", x[n],
        "are outliers"
      )
      alt_val <- c(x[n - 1], x[n])
      u <- var(x[1:(n - 2)]) / var(x) * (n - 3) / (n - 1)
    }
    g <- NULL
    pval <- pgrubbs(u, n, type = 20)
    method <- "Grubbs test for two outliers"
  }
  if (two.sided) {
    pval <- 2 * pval
    if (pval > 1) {
      pval <- 2 - pval
    }
  }
  RVAL <- list(
    statistic = c(G = g, U = u), alternative = alt,
    alt_val = alt_val,
    p.value = pval, method = method, data.name = DNAME
  )
  class(RVAL) <- "htest"
  return(RVAL)
}

grubbs <- function(x, y, alpha = 0.01) {
  v <- na.omit(c(as.numeric(x), as.numeric(y)))
  if (length(v) >= 3) {
    gt <- grubbs.test(v)
    if (gt$p.value < alpha && !is.na(gt$statistic[[1]])) {
      x[which(x == gt$alt_val)] <- NA
      y[which(y == gt$alt_val)] <- NA
      return(
        list(
          x = x, y = y,
          n = length(v),
          outliers = gt$alt_val
        )
      )
    }
  }

  return(list(
    x = x, y = y,
    n = length(v),
    outliers = NULL
  ))
}
grubbs.v <- function(v, alpha = 0.01) {
  v.copy <- na.omit(v)
  outliers <- vector()
  while (length(v.copy) >= 3) {
    gt <- grubbs.test(v.copy)
    if (gt$p.value < alpha && !is.na(gt$statistic[[1]])) {
      v.copy[which(v.copy == gt$alt_val)[1]] <- NA
      v.copy <- na.omit(v.copy)
      outliers <- c(outliers, gt$alt_val)
    } else {
      break
    }
  }
  return(
    list(v = v.copy, n = length(v.copy), outliers = outliers)
  )
}
pe.conclusion <- function(rmid, pex, pey) {
  result <- c("With measurement procedure X, ")
  upex <- unlist(pex)
  upey <- unlist(pey)
  xspe <- sum(upex == TRUE)
  xnspe <- sum(upex == FALSE)
  yspe <- sum(upey == TRUE)
  ynspe <- sum(upey == FALSE)
  if (xspe > 0) {
    id <- names(pex)[upex == TRUE]
    result <- c(
      result,
      sprintf("reference material %s exhibit significant position effects", join(id))
    )
  }
  if (xspe > 0 && xnspe > 0) {
    result <- c(result, ", and ")
  }
  if (xnspe > 0) {
    id <- names(pex)[upex == FALSE]
    result <- c(result, sprintf("reference material %s did not exhibit significant position effects", join(id)))
  }
  result <- c(result, ". ")
  result <- c(result, "\nWith measurement procedure Y, ")
  if (yspe > 0) {
    id <- names(pey)[upey == TRUE]
    result <- c(
      result,
      sprintf("reference material %s exhibit significant position effects", join(id))
    )
  }
  if (yspe > 0 && ynspe > 0) {
    result <- c(result, ", and ")
  }
  if (ynspe > 0) {
    id <- names(pex)[upey == FALSE]
    result <- c(result, sprintf("reference material %s did not exhibit significant position effects", join(id)))
  }
  result <- c(result, ".")
  return(paste0(result, collapse = ""))
}
ums <- function(rm, cs) {
  total <- rm + cs
  rm_id <- sample(seq_len(rm))
  cs_id <- sample(seq_len(cs))
  i <- 1 # index for reference material
  j <- 1 # index for clinical sample
  result <- data.frame(
    id = integer(),
    source = character(),
    stringsAsFactors = FALSE
  )

  for (pos in 1:total) {
    expected_n <- rm * pos / total
    if (i <= rm && i <= round(expected_n)) {
      result <- rbind(
        result,
        data.frame(
          id = rm_id[i],
          source = "R",
          stringsAsFactors = FALSE
        )
      )
      i <- i + 1
    } else if (j <= cs) {
      result <- rbind(
        result,
        data.frame(
          id = cs_id[j],
          source = "C",
          stringsAsFactors = FALSE
        )
      )
      j <- j + 1
    }
  }

  # Add any remaining samples if not all added
  if (i <= rm) {
    for (k in i:rm) {
      result <- rbind(
        result,
        data.frame(
          id = rm_id[k],
          source = "R",
          stringsAsFactors = FALSE
        )
      )
    }
  }
  if (j <= cs) {
    for (k in j:cs) {
      result <- rbind(
        result, data.frame(
          id = cs_id[k],
          source = "C",
          stringsAsFactors = FALSE
        )
      )
    }
  }

  return(result)
}
extract_scientific_parts <- function(x) {
  sci <- sprintf("%.15e", x)
  parts <- strsplit(sci, "e")[[1]]
  mantissa <- as.numeric(parts[1])
  exponent <- as.integer(parts[2])
  return(list(mantissa = mantissa, exponent = exponent))
}
sp <- function(x, min.exp = 3, with.tex = TRUE) {
  esp <- extract_scientific_parts(x)
  if (abs(esp$exponent) < min.exp) {
    if (with.tex) {
      return(tprintf("$%s$", ftrim(x)))
    } else {
      return(tprintf("%s", ftrim(x)))
    }
  }
  if (with.tex) {
    return(tprintf(
      "$%s \\times 10^{%s}$",
      ftrim(esp$mantissa), ftrim(esp$exponent)
    ))
  } else {
    return(tprintf(
      "%s \\times 10^{%s}",
      ftrim(esp$mantissa), ftrim(esp$exponent)
    ))
  }
}
