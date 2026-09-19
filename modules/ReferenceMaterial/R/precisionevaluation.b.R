# This file is a generated template, your changes will not be overwritten

PrecisionEvaluationClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "PrecisionEvaluationClass",
    inherit = PrecisionEvaluationBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        lcx <- na.omit(self$data[, self$options$lcx])
        lcy <- na.omit(self$data[, self$options$lcy])

        hcx <- na.omit(self$data[, self$options$hcx])
        hcy <- na.omit(self$data[, self$options$hcy])

        g <- grubbs.v(lcx, self$options$ga)
        lcxo <- length(lcx) - g$n
        if (g$n != length(lcx)) {
          self$results$og$items$o$setVisible(TRUE)
          lcx <- g$v
          for (o in g$outliers) {
            self$results$og$items$o$addRow(
              rowKey = "",
              values = list(
                mp = "X",
                l = "Low",
                v = o
              )
            )
          }
        }
        g <- grubbs.v(lcy, self$options$ga)
        lcyo <- length(lcy) - g$n
        if (g$n != length(lcy)) {
          self$results$og$items$o$setVisible(TRUE)
          lcy <- g$v
          for (o in g$outliers) {
            self$results$og$items$o$addRow(
              rowKey = "",
              values = list(
                mp = "Y",
                l = "Low",
                v = o
              )
            )
          }
        }
        g <- grubbs.v(hcx, self$options$ga)
        hcxo <- length(hcx) - g$n
        if (g$n != length(hcx)) {
          self$results$og$items$o$setVisible(TRUE)
          hcx <- g$v
          for (o in g$outliers) {
            self$results$og$items$o$addRow(
              rowKey = "",
              values = list(
                mp = "X",
                l = "High",
                v = o
              )
            )
          }
        }
        g <- grubbs.v(hcy, self$options$ga)
        hcyo <- length(hcy) - g$n
        if (g$n != length(hcy)) {
          self$results$og$items$o$setVisible(TRUE)
          hcy <- g$v
          for (o in g$outliers) {
            self$results$og$items$o$addRow(
              rowKey = "",
              values = list(
                mp = "Y",
                l = "High",
                v = o
              )
            )
          }
        }
        if (!self$results$og$items$o$visible) {
          self$results$og$items$oc$setContent("Non-declared outliers.")
        } else {
          r <- c()
          if (lcxo > 0) {
            r <- c(r, tprintf(
              "There %s %d outlier%s in low level samples on measurement procedure X.",
              ifelse(lcxo > 1, "were", "was"),
              lcxo,
              ifelse(lcxo > 1, "s", "")
            ))
          }
          if (lcyo > 0) {
            r <- c(r, tprintf(
              "There %s %d outlier%s in low level samples on measurement procedure Y.",
              ifelse(lcyo > 1, "were", "was"),
              lcyo,
              ifelse(lcyo > 1, "s", "")
            ))
          }
          if (hcxo > 0) {
            r <- c(r, tprintf(
              "There %s %d outlier%s in high level samples on measurement procedure X.",
              ifelse(hcxo > 1, "were", "was"),
              hcxo,
              ifelse(hcxo > 1, "s", "")
            ))
          }
          if (hcyo > 0) {
            r <- c(r, tprintf(
              "There %s %d outlier%s in high level samples on measurement procedure Y.",
              ifelse(hcyo > 1, "were", "was"),
              hcyo,
              ifelse(hcyo > 1, "s", "")
            ))
          }
          self$results$og$items$oc$setContent(r)
        }

        lxm <- mean(lcx, na.rm = TRUE)
        lym <- mean(lcy, na.rm = TRUE)

        hxm <- mean(hcx, na.rm = TRUE)
        hym <- mean(hcy, na.rm = TRUE)

        lxsd <- sd(lcx, na.rm = TRUE)
        lysd <- sd(lcy, na.rm = TRUE)

        hxsd <- sd(hcx, na.rm = TRUE)
        hysd <- sd(hcy, na.rm = TRUE)

        lxcv <- 100 * lxsd / lxm
        lycv <- 100 * lysd / lym

        hxcv <- 100 * hxsd / hxm
        hycv <- 100 * hysd / hym

        cxsd <- sqrt(lxsd^2 / 2 + hxsd^2 / 2)
        cysd <- sqrt(lysd^2 / 2 + hysd^2 / 2)

        self$results$p$items$t$addRow(
          rowKey = "X",
          values = list(
            mp = "X",
            lm = lxm,
            lsd = lxsd,
            lcv = lxcv,
            hm = hxm,
            hsd = hxsd,
            hcv = hxcv,
            csd = cxsd
          )
        )
        self$results$p$items$t$addRow(
          rowKey = "Y",
          values = list(
            mp = "Y",
            lm = lym,
            lsd = lysd,
            lcv = lycv,
            hm = hym,
            hsd = hysd,
            hcv = hycv,
            csd = cysd
          )
        )
        self$results$p$items$a$setContent("Where MP means measurement procedure.")
        w <- c()
        if (length(lcx) < 20) {
          self$results$w$setVisible(TRUE)
          w <- c(w, tprintf(
            "The number of valid replications for low level on measurement procedure X $%d < 20$.",
            length(lcx)
          ))
        }
        if (length(lcy) < 20) {
          self$results$w$setVisible(TRUE)
          w <- c(w, tprintf(
            "The number of valid replications for low level on measurement procedure Y $%d < 20$.",
            length(lcy)
          ))
        }
        if (length(hcx) < 20) {
          self$results$w$setVisible(TRUE)
          w <- c(w, tprintf(
            "The number of valid replications for high level on measurement procedure X $%d < 20$.",
            length(hcx)
          ))
        }
        if (length(hcy) < 20) {
          self$results$w$setVisible(TRUE)
          w <- c(w, tprintf(
            "The number of valid replications for high level on measurement procedure Y $%d < 20$.",
            length(hcy)
          ))
        }
        self$results$w$setContent(w)
      }
    )
  )
}
