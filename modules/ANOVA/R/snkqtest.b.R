# This file is a generated template, your changes will not be overwritten

SNKQTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SNKQTestClass",
    inherit = SNKQTestBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        m <- self$data[, self$options$m]
        n <- self$data[, self$options$n]
        df <- data.frame(m = m, n = n, g = rev(seq_along(m)))
        df <- df[rev(ordered(df$m)), ]
        m <- df$m
        n <- df$n
        g <- df$g
        a <- as.numeric(self$options$a)
        mse <- self$options$mse
        ve <- self$options$ve
        for (r in length(m):2) {
          qa <- qtukey(1 - a, r, ve)
          for (i in seq_along(m)) {
            if (r + i - 1 > length(m)) {
              next
            }
            j <- i + r - 1
            mi <- m[i]
            mj <- m[j]
            ni <- n[i]
            nj <- n[j]
            sij <- sqrt(mse / 2 * (1 / ni + 1 / nj))
            q <- (mi - mj) / sij
            p <- 2 * (1 - ptukey(abs(q), r, ve))
            gn <- sprintf("%d,%d", g[i], g[j])
            result <- "Reject"
            if (p >= a) {
              result <- "Not Reject"
            }
            self$results$qt$addRow(
              rowKey = gn,
              values = list(
                cg = gn,
                md = mi - mj,
                a = r,
                q = q,
                qa = qa,
                p = p,
                h0 = result
              )
            )
          }
        }
      }
    )
  )
}
