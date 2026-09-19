# This file is a generated template, your changes will not be overwritten

usClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "usClass",
    inherit = usBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        cols <- self$options$x
        time <- self$options$time
        table <- self$results$table
        df <- self$data[, cols]
        X_i <- self$data[, time]
        detail <- self$results$detail$items
        detail$Xi$setContent(X_i)
        X_mean <- mean(X_i, na.rm = TRUE)
        detail$Xm$setContent(X_mean)
        Y_i_mean <- apply(df, 1, mean, na.rm = TRUE)
        detail$Yim$setContent(Y_i_mean)
        Y_mean_mean <- mean(unlist(df))
        detail$Ymm$setContent(Y_mean_mean)
        numerator <- 0
        denominator <- 0
        n <- length(Y_i_mean)
        for (i in 1:n) {
          d_val <- X_i[i] - X_mean
          numerator <- numerator + d_val * (Y_i_mean[i] - Y_mean_mean)
          denominator <- denominator + d_val * d_val
        }
        b1 <- numerator / denominator
        detail$b1$setContent(as.numeric(b1))
        b0 <- Y_mean_mean - b1 * X_mean
        detail$b0$setContent(as.numeric(b0))
        numerator <- 0
        denominator <- n - 2
        for (i in 1:n) {
          d_val <- Y_i_mean[i] - b0 - b1 * X_i[i]
          numerator <- numerator + d_val * d_val
        }
        s <- sqrt(numerator / denominator)
        detail$s$setContent(as.numeric(s))
        denominator <- 0
        for (i in 1:n) {
          d_val <- X_i[i] - X_mean
          denominator <- denominator + d_val * d_val
        }
        sb1 <- s / sqrt(denominator)
        detail$sb1$setContent(as.numeric(sb1))
        result <- ""
        tppf <- as.numeric(qt(1 - 0.05 / 2, df = n - 2))
        t <- max(X_i)
        ud <- 0
        uts <- as.numeric(t * sb1)
        tppfsb1 <- as.numeric(tppf * sb1)
        detail$tppf$setContent(tppf)
        detail$tppfsb1$setContent(tppfsb1)
        if (abs(b1) < tppfsb1) {
          result <- sprintf("Stable, uncertainty component$u_\\{ts\\}$=%f", uts)
        } else if (abs(b1) >= tppfsb1) {
          if (t * sb1 <= ud / 3) {
            result <- sprintf("Relatively stable, uncertainty component$u_\\{ts\\}$=%f", uts)
          } else {
            result <- sprintf("Calibrator needs re-preparation or shelf-life should be shortened, uncertainty component$u_\\{s\\}$=%f", uts)
          }
        }
        self$results$result$setContent(result)
        detail$uts$setContent(uts)
        table$setRow(
          rowNo = 1,
          value = list(
            b1 = b1,
            b0 = b0,
            s = s,
            sb1 = sb1,
            tppf = tppf,
            tppfsb1 = tppfsb1
          )
        )
      }
    )
  )
}
