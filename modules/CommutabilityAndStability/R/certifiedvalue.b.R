# This file is a generated template, your changes will not be overwritten
library(outliers)
"grubbs.test.alted" <-
  function(x, type = 10, opposite = FALSE, two.sided = FALSE) {
    if (sum(c(10, 11, 20) == type) == 0) stop("Incorrect type")
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])

    n <- length(x)

    if (type == 11) {
      g <- (x[n] - x[1]) / sd(x)
      u <- var(x[2:(n - 1)]) / var(x) * (n - 3) / (n - 1)

      pval <- 1 - pgrubbs(g, n, type = 11)

      method <- "Grubbs test for two opposite outliers"

      # alt <- paste(x[1], "and", x[n], "are outliers")
      alt <- c(x[1], x[n])
    } else if (type == 10) {
      if (xor(((x[n] - mean(x)) < (mean(x) - x[1])), opposite)) {
        # alt <- paste("lowest value", x[1], "is an outlier")
        alt <- c(x[1])
        o <- x[1]
        d <- x[2:n]
      } else {
        # alt <- paste("highest value", x[n], "is an outlier")
        alt <- c(x[n])
        o <- x[n]
        d <- x[1:(n - 1)]
      }

      g <- abs(o - mean(x)) / sd(x)
      u <- var(d) / var(x) * (n - 2) / (n - 1)

      pval <- 1 - pgrubbs(g, n, type = 10)

      method <- "Grubbs test for one outlier"
    } else {
      if (xor(((x[n] - mean(x)) < (mean(x) - x[1])), opposite)) {
        # alt <- paste("lowest values", x[1], ",", x[2], "are outliers")
        alt <- c(x[1], x[2])
        u <- var(x[3:n]) / var(x) * (n - 3) / (n - 1)
      } else {
        # alt <- paste("highest values", x[n - 1], ",", x[n], "are outliers")
        alt <- c(x[n - 1], x[n])
        u <- var(x[1:(n - 2)]) / var(x) * (n - 3) / (n - 1)
      }

      g <- NULL

      pval <- pgrubbs(u, n, type = 20)

      method <- "Grubbs test for two outliers"
    }

    if (two.sided) {
      pval <- 2 * pval
      if (pval > 1) pval <- 2 - pval
    }


    RVAL <- list(
      statistic = c(G = g, U = u),
      alternative = alt, p.value = pval, method = method,
      data.name = DNAME
    )
    class(RVAL) <- "htest"
    return(RVAL)
  }
CertifiedValueClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "CertifiedValueClass",
    inherit = CertifiedValueBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        a <- as.numeric(self$options$a)
        x <- unlist(x)
        str <- shapiro.test(x)
        self$results$st$setTitle(
          str$method
        )
        self$results$st$setContent(
          c(
            sprintf("$H_0$: %sReject", ifelse(str$p.value <= a, "", "Not ")),
            sprintf("$P=%f$", str$p.value)
          )
        )
        ktr <- ks.test(x, "pnorm", mean(x), sd(x))
        self$results$kst$setTitle(
          ktr$method
        )
        self$results$kst$setContent(
          c(
            sprintf("$H_0$: %sReject", ifelse(ktr$p.value <= a, "", "Not ")),
            sprintf("$P=%f$", ktr$p.value)
          )
        )
        olts <- c()
        while (TRUE) {
          gtr <- grubbs.test.alted(x, type = 10)
          if (gtr$p.value <= a) {
            olts <- append(gtr$alternative, olts)
            x <- x[x != gtr$alternative]
          } else {
            break
          }
        }
        gtrc <- c("Outlies:", olts)
        if (length(olts) < 1) {
          gtrc <- c(
            sprintf("$P=%f$", gtr$p.value),
            "No need for exclusion"
          )
        }
        self$results$gtr$setContent(
          gtrc
        )
        sd <- sd(x)
        m <- mean(x)
        self$results$cv$setContent(
          c(
            sprintf("$C=%f$", m),
            sprintf("$SD=%f$", sd),
            sprintf("$CV=%f\\%%$", sd / m*100)
          )
        )
      }
    )
  )
}
