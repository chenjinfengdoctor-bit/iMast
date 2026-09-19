# This file is a generated template, your changes will not be overwritten

OneFactorANOVAClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "OneFactorANOVAClass",
    inherit = OneFactorANOVABase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        xl <- na.omit(unlist(x))
        xl2 <- xl^2
        C <- sum(xl)^2 / length(xl)
        ss <- sum(xl2) - C
        v <- length(xl) - 1
        self$results$anova$addRow(
          rowKey = "Total Variation",
          values = list(
            sv = "Total Variation",
            v = v,
            ss = ss,
            ms = "",
            f = "",
            p = ""
          )
        )
        v_bg <- ncol(x) - 1
        ss_bg <- sum(apply(x, 2, function(col) {
          col <- na.omit(col)
          sum(col)^2 / length(col)
        })) - C
        v_wg <- v - v_bg
        ss_wg <- ss - ss_bg
        ms_bg <- ss_bg / v_bg
        ms_wg <- ss_wg / v_wg
        f_bg <- ms_bg / ms_wg
        p <- 1 - pf(f_bg, v_bg, v_wg)
        a <- as.numeric(self$options$a)
        self$results$anova$addRow(
          rowKey = "Between Group",
          values = list(
            sv = "Between Group",
            v = v_bg,
            ss = ss_bg,
            ms = ms_bg,
            f = f_bg,
            p = p
          )
        )
        self$results$anova$addRow(
          rowKey = "Within Group",
          values = list(
            sv = "Within Group",
            v = v_wg,
            ss = ss_wg,
            ms = ms_wg,
            f = "",
            p = ""
          )
        )
        if (p <= a) {
          self$results$conclusion$setTitle(
            "$H_0(\\mu_1=\\cdots=\\mu_n)$: Reject"
          )
        } else {
          self$results$conclusion$setTitle(
            "$H_0(\\mu_1=\\cdots=\\mu_n)$: Not Reject"
          )
        }
        self$results$conclusion$setContent(
          sprintf(
            "$F_\\{%.2f,(%d,%d)\\}=%f$",
            a, v_bg, v_wg, qf(1 - a, v_bg, v_wg)
          )
        )
      }
    )
  )
}
