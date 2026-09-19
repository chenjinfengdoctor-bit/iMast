# This file is a generated template, your changes will not be overwritten

PEOfNBDClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "PEOfNBDClass",
    inherit = PEOfNBDBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        f <- self$data[, self$options$f]
        N <- sum(f)
        x <- self$data[, self$options$x]
        ox <- order(x)
        x <- x[ox]
        f <- f[ox]
        a <- as.numeric(self$options$a)
        if (length(x) != max(x) + 1) {
          return()
        }
        m <- sum(f * x) / N
        k <- NA
        sk <- self$options$sk
        k1 <- self$options$k1
        k2 <- self$options$k2
        add <- ""
        Ax <- sapply(x, function(v) sum(f[x > v]))
        if (self$options$method == "m") {
          S2 <- (sum(f * x^2) - sum(f * x)^2 / N) / (N - 1)
          k <- m^2 / (S2 - m)
        } else if (self$options$method == "zf") {
          f0 <- f[x == 0]
          A <- log10(N / f0)
          func <- function(k) k * log10(1 + m / k) - A
          if (!sk) {
            lower <- 1
            upper <- 1
            while (func(lower) > 0) {
              lower <- lower / 2
            }
            while (func(upper) < 0) {
              upper <- upper * 2
            }

            k <- uniroot(
              func,
              lower = lower, upper = upper
            )$root
          } else {
            z1 <- func(k1)
            z2 <- func(k2)
            if (z1 < 0 && z2 > 0) {
              k <- k1 + (k2 - k1) * (-z1) / (z2 - z1)
            } else {
              k <- NA
            }
          }
        } else if (self$options$method == "ml") {
          func <- function(k) sum(Ax / (k + x)) - N * log(1 + m / k)
          if (!sk) {
            lower <- 1
            upper <- 1
            while (func(lower) < 0) {
              lower <- lower / 2
            }
            while (func(upper) > 0) {
              upper <- upper * 2
            }
            k <- uniroot(
              func,
              lower = lower, upper = upper
            )$root
          } else {
            z1 <- func(k1)
            z2 <- func(k2)
            add <- sprintf("\\\\ z_1=%f \\\\ z_2=%f", z1, z2)
            if (z1 * z2 < 0) {
              k <- k1 - (k2 - k1) / (z2 - z1) * z1
            } else {
              k <- NA
            }
          }
        }
        self$results$result$setTitle(
          sprintf("$\\hat \\mu=%f \\\\ \\hat k=%f$", m, k)
        )
        self$results$result$setContent(
          sprintf("$\\overline X=%f %s$", m, add)
        )
        p <- m / k
        q <- 1 + p
        px <- rep(0, max(x) + 1)
        px[1] <- q^(-k)
        for (i in 2:max(x)) {
          ix <- i - 1
          px[i] <- px[i - 1] * (k + ix - 1) * p / ix / q
        }
        px[length(px)] <- 1 - sum(px[1:(length(px) - 1)])
        T <- N * px
        chi2 <- (T - f)^2 / T
        dt <- data.frame(
          X = x, f = f, Ax = Ax, PX = px, T = T, chi2 = chi2
        )
        small_rows <- dt[dt$T < 5, ]
        merged_row <- data.frame(
          X = paste(small_rows$X, collapse = ","),
          f = sum(small_rows$f),
          Ax = sum(small_rows$Ax),
          PX = sum(small_rows$PX),
          T = sum(small_rows$T),
          chi2 = sum(small_rows$chi2)
        )
        dtm <- dt[dt$T >= 5, ]
        dtm <- rbind(dtm, merged_row)
        dtm$chi2 <- (dtm$T - dtm$f)^2 / dtm$T

        v <- nrow(dt) - nrow(small_rows) + 1 - 3
        P <- 1 - pchisq(sum(dtm$chi2), v)
        h0 <- ": Not Reject"
        if (P <= a) {
          h0 <- ": Reject"
        }
        self$results$goft$setContent(
          sprintf(
            paste0(
              "$H_0$(Follow Negative Binomial Distribution)",
              h0,
              "\n$\\chi^2=%f  \\\\ ",
              "v=%d \\\\ P=%f",
              "$\n", paste(
                capture.output(print(dtm)),
                collapse = "\n"
              )
            ), sum(dtm$chi2), v, P
          )
        )
      }
    )
  )
}
