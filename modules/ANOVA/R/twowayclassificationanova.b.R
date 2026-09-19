# This file is a generated template, your changes will not be overwritten
library(MASS)
TwoWayClassificationANOVAClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "TwoWayClassificationANOVAClass",
    inherit = TwoWayClassificationANOVABase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        xl <- unlist(x)
        N <- length(xl)
        v <- N - 1
        xl2 <- xl^2
        C <- sum(xl)^2 / N
        ss <- sum(xl2) - C
        g <- ncol(x)
        n <- nrow(x)
        v_g <- g - 1
        ss_g <- sum(
          apply(x, 2, function(col) {
            sum(col)^2
          })
        ) / n - C
        ms_g <- ss_g / v_g
        n <- nrow(x)
        v_n <- n - 1
        ss_n <- sum(apply(x, 1, function(row) {
          sum(row)^2
        })) / g - C
        ms_n <- ss_n / v_n
        v_e <- (n - 1) * (g - 1)
        ss_e <- ss - ss_g - ss_n
        ms_e <- ss_e / v_e
        f_g <- ms_g / ms_e
        f_n <- ms_n / ms_e
        p_g <- 1 - pf(f_g, v_g, v_e)
        p_n <- 1 - pf(f_n, v_g, v_e)
        a <- as.numeric(self$options$a)
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
        self$results$anova$addRow(
          rowKey = "Between-Cluster",
          values = list(
            sv = "Between-Cluster",
            v = v_g,
            ss = ss_g,
            ms = ms_g,
            f = f_g,
            p = p_g
          )
        )
        self$results$anova$addRow(
          rowKey = "Between-Block",
          values = list(
            sv = "Between-Block",
            v = v_n,
            ss = ss_n,
            ms = ms_n,
            f = f_n,
            p = p_n
          )
        )
        self$results$anova$addRow(
          rowKey = "Error",
          values = list(
            sv = "Error",
            v = v_e,
            ss = ss_e,
            ms = ms_e,
            f = "",
            p = ""
          )
        )
        if (p_g <= a || p_n <= a) {
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
            a, v_g, v_e, qf(1 - a, v_g, v_e)
          )
        )
      }
    )
  )
}
