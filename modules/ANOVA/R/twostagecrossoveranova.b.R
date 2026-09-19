# This file is a generated template, your changes will not be overwritten

TwoStageCrossoverANOVAClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "TwoStageCrossoverANOVAClass",
    inherit = TwoStageCrossoverANOVABase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        k <- self$data[, self$options$k]

        xl <- unlist(x)
        kl <- unlist(k)
        ukc <- sort(unique(kl))
        t_k <- rep(0, length(ukc))
        t_k <- setNames(t_k, ukc)
        for (i in seq_along(xl)) {
          kv <- kl[[i]]
          t_k[kv] <- t_k[kv] + xl[[i]]
        }
        m_k <- t_k / nrow(x)
        N <- length(xl)
        g <- nrow(x)
        v_t <- N - 1
        C <- sum(x)^2 / N
        ss_t <- sum(x^2) - C
        v_k <- length(ukc) - 1
        ss_k <- (t_k[1] - t_k[2])^2 / N
        ms_k <- ss_k / v_k
        v_c <- ncol(x) - 1
        ss_c <- (sum(x[, 1]) - sum(x[, 2]))^2 / N
        ms_c <- ss_c / v_c
        v_r <- g - 1
        ss_r <- sum(rowSums(x)^2) / 2 - C
        ms_r <- ss_r / v_r

        v_e <- v_k * (v_r - 1)
        ss_e <- ss_t - ss_k - ss_r - ss_c
        ms_e <- ss_e / v_e

        f_k <- ms_k / ms_e
        f_r <- ms_r / ms_e
        f_c <- ms_c / ms_e

        p_k <- 1 - pf(f_k, v_k, v_e)
        p_r <- 1 - pf(f_r, v_r, v_e)
        p_c <- 1 - pf(f_c, v_c, v_e)
        self$results$anova$addRow(
          rowKey = "Total Variation",
          values = list(
            sv = "Total Variation",
            v = v_t,
            ss = ss_t,
            ms = "",
            f = "",
            p = ""
          )
        )
        self$results$anova$addRow(
          rowKey = "Treatment",
          values = list(
            sv = "Treatment",
            v = v_k,
            ss = ss_k,
            ms = ms_k,
            f = f_k,
            p = p_k
          )
        )

        self$results$anova$addRow(
          rowKey = "Column",
          values = list(
            sv = "Column",
            v = v_c,
            ss = ss_c,
            ms = ms_c,
            f = f_c,
            p = p_c
          )
        )
        self$results$anova$addRow(
          rowKey = "Row",
          values = list(
            sv = "Row",
            v = v_r,
            ss = ss_r,
            ms = ms_r,
            f = f_r,
            p = p_r
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
        a <- as.numeric(self$options$a)
        dots <- "=\\cdots"
        if (length(ukc) <= 2) {
          dots <- ""
        }
        if (p_k <= a) {
          self$results$h0$setTitle(
            sprintf("$H_0(\\mu_1%s=\\mu_%d)$: Reject", dots, length(ukc))
          )
        } else {
          self$results$h0$setTitle(
            sprintf("$H_0(\\mu_1%s=\\mu_%d)$: Not Reject", dots, length(ukc))
          )
        }
        self$results$h0$setContent(
          sprintf(
            "$F_\\{%.2f,(%d,%d)\\}=%f$",
            a, v_k, v_e, qf(1 - a, v_k, v_e)
          )
        )
      }
    )
  )
}
