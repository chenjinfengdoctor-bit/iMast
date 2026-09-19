# This file is a generated template, your changes will not be overwritten
library(e1071)
library(nortest)
library(moments)
library(DescTools)
library(lawstat)
conover.test <- function(x) {
  K <- length(x)
  n <- sapply(x, length)
  N <- sum(n)
  Z <- sapply(x, function(y) abs(y - mean(y)))
  R_all <- rank(unlist(Z))
  index <- 1
  R <- lapply(Z, function(z) {
    l <- length(z)
    result <- R_all[index:(index + l - 1)]
    index <<- index + l
    return(result)
  })
  S <- sapply(R, function(r) sum(r^2))
  Sm <- sum(S) / N
  D2 <- 1 / (N - 1) * (sum(sapply(R, function(r) {
    sum(r^4 - Sm^2)
  })))
  T <- 1 / D2 * (sum(S^2 / n) - N * Sm^2)
  return(list(statistic = T, p.value = 1 - pchisq(T, K - 1)))
}
mi.test <- function(x) {
  M <- median(x)
  n <- length(x)
  aux1 <- x - M
  xtmp <- abs(aux1)
  A <- 9.0 * median(xtmp)
  z <- aux1 / A
  term1 <- 0
  term2 <- 0
  term3 <- sum(aux1^2)
  for (i in 1:n) {
    if (abs(z[i]) < 1) {
      z2 <- z[i]^2
      term1 <- term1 + aux1[i]^2 * (1 - z2)^4
      term2 <- term2 + (1 - z2) * (1 - 5 * z2)
    }
  }
  Sb2 <- n * term1 / term2^2
  statIn <- (term3 / (n - 1)) / Sb2
  statIn
}
mi.cv <- function(n) {
  m <- as.integer(500)
  cv <- numeric(m)
  for (i in 1:m) {
    data <- rnorm(n)
    stat <- mi.test(data)
    cv[i] <- stat
  }
  cv
}
prb <- function(r, s, a) {
  midr <- median(r)
  y <- (r - midr) / midr
  x <- log(0.5 * (s + a / midr)) / log(2)
  model <- lm(y ~ x)
  slope <- coef(model)[2]
  slope
}
dk.test <- function(x) {
  n <- length(x)
  mx <- mean(x)
  m4 <- sum((x - mx)^4) / n
  m2 <- sum((x - mx)^2) / n
  b2 <- m4 / m2^2
  G <- (b2 - (3 * n - 3) / (n + 1)) /
    sqrt(
      24 * n * (n - 2) * (n - 3) /
        (n + 1)^2 / (n + 3) / (n + 5)
    )
  E <- 6 * (n^2 - 5 * n + 2) / (n + 7) / (n + 9) *
    sqrt(6 * (n + 3) * (n + 5) / n / (n - 2) / (n - 3))
  A <- 6 + 8 / E * (2 / E + sqrt(1 + 4 / E^2))
  zk <- ((1 - 2 / 9 / A) - (
    (1 - 2 / A) /
      (1 + G * sqrt(2 / (A - 4)))
  )^(1 / 3)) /
    sqrt(2 / 9 / A)
  return(zk)
}
sk.process <- function(res, alpha, hetan) {
  result <- agostino.test(unlist(res))
  hetan_sv <- result$statistic[2]
  hetan_sp <- result$p.value
  hetan$addRow(
    rowKey = "ST",
    values = list(
      na = "Skewness Test", tv = hetan_sv, p = hetan_sp,
      rn = ifelse(hetan_sp <= alpha, "Yes", "No")
    )
  )
  hetan_sv
}
dk.process <- function(res, alpha, hetan) {
  hetan_kv <- dk.test(unlist(res))
  hetan_kp <- 2 * (1 - pnorm(abs(hetan_kv)))
  hetan$addRow(
    rowKey = "KT",
    values = list(
      na = "Kurtosis Test", tv = hetan_kv, p = hetan_kp,
      rn = ifelse(hetan_kp <= alpha, "Yes", "No")
    )
  )
  hetan_kv
}
ov.process <- function(sv, kv, alpha, hetan) {
  hetan_ov <- sv^2 + kv^2
  hetan_op <- 1 - pchisq(hetan_ov, df = 2)
  hetan$addRow(
    rowKey = "SK",
    values = list(
      na = "Skewness and Kurtosis (Omnibus)",
      tv = hetan_ov, p = hetan_op,
      rn = ifelse(hetan_op <= alpha, "Yes", "No")
    )
  )
}
bf.process <- function(r, group, alpha, hetaev) {
  result <- LeveneTest(unlist(r), group, center = median)
  hetaev_bfv <- result[["F value"]][1]
  hetaev_bfp <- result[["Pr(>F)"]][1]
  hetaev$addRow(
    rowKey = "BF",
    values = list(
      tn = "Brown-Forsythe (Data - Medians)",
      tv = hetaev_bfv,
      p = hetaev_bfp,
      rev = ifelse(hetaev_bfp <= alpha, "Yes", "No")
    )
  )
}
l.process <- function(r, group, alpha, hetaev) {
  result <- LeveneTest(unlist(r), group, center = mean)
  hetaev_bfv <- result[["F value"]][1]
  hetaev_bfp <- result[["Pr(>F)"]][1]
  hetaev$addRow(
    rowKey = "L",
    values = list(
      tn = "Levene (Data - Means)",
      tv = hetaev_bfv,
      p = hetaev_bfp,
      rev = ifelse(hetaev_bfp <= alpha, "Yes", "No")
    )
  )
}
conover.process <- function(res, alpha, hetaev) {
  result <- conover.test(res)
  hetaev_cv <- result$statistic
  hetaev_cp <- result$p.value
  hetaev$addRow(
    rowKey = "C",
    values = list(
      tn = "Conover (Ranks of Deviations)",
      tv = hetaev_cv,
      p = hetaev_cp,
      rev = ifelse(hetaev_cp <= alpha, "Yes", "No")
    )
  )
}
bartlett.process <- function(r, alpha, hetaev) {
  result <- bartlett.test(r)
  hetaev_bv <- result$statistic
  hetaev_bp <- result$p.value
  hetaev$addRow(
    rowKey = "B",
    values = list(
      tn = "Bartlett (Likelihood Ratio)",
      tv = hetaev_bv,
      p = hetaev_bp,
      rev = ifelse(hetaev_bp <= alpha, "Yes", "No")
    )
  )
}
kruskal.process <- function(r, alpha, theg) {
  result <- kruskal.test(r)
  H <- result$statistic
  theg_krv <- H
  theg_krp <- result$p.value
  theg$addRow(
    rowKey = "KR",
    values = list(
      tn = "Kruskal-Wallis Rank Test",
      ts = theg_krv,
      p = theg_krp,
      c = ifelse(theg_krp <= alpha, "Reject Equality of Ratio Medians",
        "Not Reject Equality of Ratio Medians"
      )
    )
  )
}
aovf.process <- function(r, group, alpha, theg) {
  result <- summary(aov(
    r ~ group, data.frame(r = unlist(r), group = group)
  ))
  theg_fv <- result[[1]][["F value"]][1]
  theg_fp <- result[[1]][["Pr(>F)"]][1]
  theg$addRow(
    rowKey = "F",
    values = list(
      tn = "ANOVA F-Test",
      ts = theg_fv,
      p = theg_fp,
      c = ifelse(theg_fp <= alpha, "Reject Equality of Ratio Means",
        "Not Reject Equality of Ratio Means"
      )
    )
  )
}
welch.process <- function(r, group, alpha, theg) {
  result <- oneway.test(r ~ group, data.frame(r = unlist(r), group = group))
  theg_wv <- result$statistic
  theg_wp <- result$p.value
  theg$addRow(
    rowKey = "W",
    values = list(
      tn = "Welch's Test",
      ts = theg_wv,
      p = theg_wp,
      c = ifelse(theg_wp <= alpha, "Reject Equality of Ratio Means",
        "Not Reject Equality of Ratio Means"
      )
    )
  )
}
AppraisalRatioStudiesClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "AppraisalRatioStudiesClass",
    inherit = AppraisalRatioStudiesBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        a <- self$data[, self$options$a]
        s <- self$data[, self$options$s]
        # sk <- as.integer(self$options$sk)
        sk <- 2
        group <- self$data[, self$options$group]
        alpha <- self$options$alpha
        df <- data.frame(a, s, group)
        dl <- split(df, df$group)
        acoc <- self$options$coc / 100
        atm <- self$options$tm / 100
        qtype <- 6
        if (sk == 1) {
          self$results$na$columns$skew$setTitle("Skew $(g_1)$")
          self$results$na$columns$kurt$setTitle("Kurt $(g_2)$")
        }
        if (sk == 2) {
          self$results$na$columns$skew$setTitle("Skew $(G_1)$")
          self$results$na$columns$kurt$setTitle("Kurt $(G_2)$")
        }
        if (sk == 3) {
          self$results$na$columns$skew$setTitle("Skew $(b_1)$")
          self$results$na$columns$kurt$setTitle("Kurt $(b_2)$")
        }
        process <- function(gd, gn, offset, do_iqrm = TRUE) {
          a <- gd$a
          s <- gd$s
          r <- a / s
          iqr <- IQR(r, type = qtype)
          if (do_iqrm & self$options$iqr) {
            iqrm <- self$options$iqrm
            lower <- quantile(r,
              probs = 0.25,
              type = qtype
            ) - iqrm * iqr
            upper <- quantile(r,
              probs = 0.75,
              type = qtype
            ) + iqrm * iqr
            ir <- which(r < lower | r > upper)
            if (length(ir) > 0) {
              self$results$rr$setVisible(TRUE)
              for (i in ir) {
                
                self$results$rr$addRow(
                  rowKey = i,
                  values = list(
                    r = i + offset,
                    group = gn,
                    rr = r[[i]],
                    reason = ifelse(r[[i]] < lower,
                      sprintf("<%f", lower),
                      sprintf(">%f", upper)
                    )
                  )
                )
              }
              r <- r[-ir]
              a <- a[-ir]
              s <- s[-ir]
            }
          }

          minr <- min(r)
          maxr <- max(r)
          mr <- mean(r)
          wm <- sum(a) / sum(s)
          midr <- median(r)

          sd <- sd(r)
          n <- length(r)
          cod <- 100 / n * sum(abs(r - midr)) / midr
          cov <- 100 * sd / mr
          prd <- mr / wm
          prb <- prb(r, s, a)
          coc <- 100 * sum(
            r >= midr * (1 - acoc) &
              r <= midr * (1 + acoc)
          ) / length(r)
          codw <- 100 / midr * sum((s / mean(s)) * abs(r - midr)) / n
          covw <- 100 / mr * sqrt(sum(s / mean(s) * (r - mr)^2) / n)
          madm <- median(abs(r - midr))
          mapdm <- madm / midr * 100
          tm <- mean(r, trim = atm)
          cimr <- NULL
          midci <- MedianCI(r, conf.level = 1 - alpha)
          mci <- c(
            mr - qt(1 - alpha / 2, n - 1) * sd / sqrt(n),
            mr + qt(1 - alpha / 2, n - 1) * sd / sqrt(n)
          )
          mciw <- c(
            wm - qt(1 - alpha / 2, n - 1) *
              sqrt(sum(a^2) - 2 * wm * sum(a * s) + wm^2 * sum(s^2)) /
              mean(s) / sqrt(n * (n - 1)),
            wm + qt(1 - alpha / 2, n - 1) *
              sqrt(sum(a^2) - 2 * wm * sum(a * s) + wm^2 * sum(s^2)) /
              mean(s) / sqrt(n * (n - 1))
          )
          skewness <- e1071::skewness(r, type = sk)
          kurtosis <- e1071::kurtosis(r, type = sk)
          swp <- shapiro.test(r)$p.value
          adp <- ad.test(r)$p.value
          mis <- mi.test(r)
          cv <- mi.cv(length(r))
          micv <- quantile(cv, 1 - alpha)
          mip <- 1 - ecdf(cv)(mis)
          ksp <- lillie.test(r)$p.value
          ds <- agostino.test(r)
          dss <- ds$statistic[2]
          dsp <- ds$p.value
          dks <- dk.test(r)
          dkp <- 2 * (1 - pnorm(abs(dks)))
          dos <- dss^2 + dks^2
          dop <- 1 - pchisq(dos, df = 2)
          y <- (r - midr) / midr
          x <- log(0.5 * (s + a / midr)) / log(2)
          model <- lm(y ~ x)
          slope <- coef(model)[2]
          sm <- summary(model)
          se <- sm$coefficients[2, "Std. Error"]
          tv <- sm$coefficients[2, "t value"]
          p <- sm$coefficients[2, "Pr(>|t|)"]
          ci <- confint(model, level = 1 - alpha)[2, ]
          self$results$rss$addRow(
            rowKey = gn,
            values = list(
              group = gn,
              count = length(r),
              median = midr,
              mean = mr,
              wtd.mean = wm,
              iqr = iqr,
              sd = sd,
              cod = cod,
              cov = cov,
              prd = prd,
              prb = prb
            )
          )
          self$results$arss$addRow(
            rowKey = gn,
            values = list(
              group = gn,
              count = length(r),
              min = minr,
              max = maxr,
              range = maxr - minr,
              coc = coc,
              wtd.cod = codw,
              wtd.cov = covw,
              madm = madm,
              mapdm = mapdm,
              trim.mean = tm
            )
          )
          self$results$aspss$addRow(
            rowKey = gn,
            values = list(
              group = gn,
              count = length(r),
              amean = mean(a),
              spmean = mean(s),
              amid = median(a),
              spmid = median(s)
            )
          )
          self$results$ci$addRow(
            rowKey = gn,
            values = list(
              group = gn,
              count = length(r),
              rmid = midr,
              midl = midci[2],
              midr = midci[3],
              rmean = mr,
              meanl = mci[1],
              meanr = mci[2],
              rwm = wm,
              wml = mciw[1],
              wmr = mciw[2]
            )
          )
          self$results$na$addRow(
            rowKey = gn,
            values = list(
              group = gn,
              count = length(r),
              skew = skewness,
              kurt = kurtosis,
              sw = swp,
              ad = adp,
              mi = mip,
              ks = ksp,
              ds = dsp,
              dk = dkp,
              do = dop
            )
          )

          self$results$prbd$addRow(
            rowKey = gn,
            values = list(
              group = gn,
              c = length(r),
              pc = cor(x, y, method = "pearson"),
              prb = slope,
              se = se,
              tv = tv,
              p = p,
              rve = ifelse(p <= alpha, "Yes", "No"),
              cil = ci[[1]],
              cir = ci[[2]]
            )
          )
          return(list(
            res = r - mr, r = r,
            group = rep(gn, length(r)),
            a = a, s = s
          ))
        }

        res <- list()
        r <- list()
        group <- list()
        a <- list()
        s <- list()
        offset <- 0
        for (gn in names(dl)) {
          result <- process(dl[[gn]], gn, offset)
          offset <- offset + nrow(dl[[gn]])
          res[[gn]] <- result$res
          r[[gn]] <- result$r
          group[[gn]] <- result$group
          a[[gn]] <- result$a
          s[[gn]] <- result$s
        }
        process(list(a = unlist(a), s = unlist(s)), "Combined", offset, FALSE)
        sv <- sk.process(res, alpha, self$results$hetan)
        kv <- dk.process(res, alpha, self$results$hetan)
        ov.process(sv, kv, alpha, self$results$hetan)
        bf.process(r, unlist(group), alpha, self$results$hetaev)
        l.process(r, unlist(group), alpha, self$results$hetaev)
        conover.process(res, alpha, self$results$hetaev)
        bartlett.process(r, alpha, self$results$hetaev)
        kruskal.process(r, alpha, self$results$theg)
        aovf.process(r, unlist(group), alpha, self$results$theg)
        welch.process(r, unlist(group), alpha, self$results$theg)
      }
    )
  )
}
