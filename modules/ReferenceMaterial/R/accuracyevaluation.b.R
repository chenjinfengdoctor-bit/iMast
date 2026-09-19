# This file is a generated template, your changes will not be overwritten

AccuracyEvaluationClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "AccuracyEvaluationClass",
    inherit = AccuracyEvaluationBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        y <- self$data[, self$options$y]
        l <- as.character(self$data[, self$options$l])
        lr <- c()
        r <- c()
        ur <- c()
        for (item in self$options$rs) {
          if (!is.null(item$l)) {
            lr <- c(lr, item$l)
            r <- c(r, item$r)
            ur <- c(ur, item$u)
          }
        }
        lr <- as.character((lr))
        r <- setNames(r, lr)
        ur <- setNames(ur, lr)
        df <- data.frame(l = lr, r = r, u = ur)
        df <- df[!duplicated(df$l) & !is.na(df$l), ]
        nr <- setNames(df$r, df$l)
        nu <- setNames(df$u, df$l)
        x.g <- split(x, l)
        y.g <- split(y, l)
        mx <- sapply(x.g, mean)
        my <- sapply(y.g, mean)
        sdx <- sapply(x.g, sd)
        sdy <- sapply(y.g, sd)
        rdx <- setNames(sapply(lr, function(level) {
          100 * (mx[[level]] - nr[[level]]) / nr[[level]]
        }), df$l)
        rdy <- setNames(sapply(lr, function(level) {
          100 * (my[[level]] - nr[[level]]) / nr[[level]]
        }), df$l)
        c <- c()
        t.x.a <- TRUE
        t.y.a <- TRUE
        small.sample <- FALSE
        for (l in lr) {
          if (length(x.g[[l]]) < 5 || length(y.g[[l]]) < 5) {
            small.sample <- TRUE
          }
          self$results$t$addRow(
            rowKey = l,
            values = list(
              l = l,
              v = tprintf(
                "$%s\\pm%s, k=%s$",
                ftrim(nr[[l]]), ftrim(nu[[l]]), ftrim(self$options$cf)
              ),
              mx = mx[[l]],
              my = my[[l]],
              sdx = sdx[[l]],
              sdy = sdy[[l]],
              rdx = rdx[[l]],
              rdy = rdy[[l]]
            )
          )
          x.a <- abs(rdx[[l]]) < abs(self$options$c)
          y.a <- abs(rdy[[l]]) < abs(self$options$c)
          t.x.a <- t.x.a && x.a
          t.y.a <- t.y.a && y.a
          l <- paste0(tolower(substr(l, 1, 1)), substr(l, 2, nchar(l)))
          if (x.a && y.a) {
            c.l <- tprintf("The bias of level %s sample is allowable on both measurement procedures.", l)
          } else if ((!x.a) && (!y.a)) {
            c.l <- tprintf("The bias of level %s sample is not allowable on both measurement procedures.", l)
          } else if (x.a && !y.a) {
            c.l <- tprintf("The bias of level %s sample is allowable on the measurement procedure X, however which is not allowable on the measurement procedure Y.", l)
          } else if ((!x.a) && y.a) {
            c.l <- tprintf("The bias of level %s sample is not allowable on the measurement procedure X, however which is allowable on the measurement procedure Y.", l)
          }
          c <- c(c, c.l)
        }
        n <- c(tprintf("$k$: Coverage factor; MP: Measurement procedure; $RD$: Relative deviaion."))
        if (self$options$unit != "") {
          n <- c(tprintf("Unit: %s; ", self$options$unit), n)
        }
        n <- c("*", n)
        self$results$n$setContent(paste0(n, collapse = ""))
        self$results$c$setContent(c)
        w <- c()
        if (!(t.x.a && t.y.a)) {
          self$results$w$setVisible(TRUE)
          if (!(t.x.a || t.y.a)) {
            w <- c(w, tprintf(
              "As one or more of the relative deviation values exceed the $\\pm%s\\%%$ threshold, both measurement procedures were considered to be unsuitable for communtability assessments.",
              ftrim(abs(self$options$c))
            ))
          } else if (!t.x.a) {
            w <- c(w, tprintf(
              "As one or more of the relative deviation values exceed the $\\pm%s\\%%$ threshold, the measurement procedure X were considered to be unsuitable for communtability assessments.",
              ftrim(abs(self$options$c))
            ))
          } else if (!t.y.a) {
            w <- c(w, tprintf(
              "As one or more of the relative deviation values exceed the $\\pm%s\\%%$ threshold, the measurement procedure Y were considered to be unsuitable for communtability assessments.",
              ftrim(abs(self$options$c))
            ))
          }
        }
        if (small.sample) {
          self$results$w$setVisible(TRUE)
          w <- c(w, "To ensure statistical power, the number of measured repetitions for each level is recommended to be no less than 5 times.")
        }
        self$results$w$setContent(w)
      }
    )
  )
}
