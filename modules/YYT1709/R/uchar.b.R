# This file is a generated template, your changes will not be overwritten

ucharClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "ucharClass",
    inherit = ucharBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        df <- self$data[, self$options$x]
        detail <- self$results$detail$items
        Uwcal <- self$options$Uwcal
        k <- self$options$k
        Cwcal <- self$options$Cwcal
        C <- self$options$C
        Uwcalrel <- Uwcal / (k * Cwcal)
        if (is.nan(Uwcalrel)) {
          Uwcalrel <- 0
        }
        Ureprel <- 0
        Uorel <- self$options$Uorel
        detail$Uwcalrel$setContent(Uwcalrel)
        m <- nrow(df)
        n <- ncol(df)
        W_i <- apply(
          df, 1,
          function(x) 1 / (sd(x) / sqrt(sum(!is.na(x)))^2)
        )
        if (self$options$single) {
          numerator <- sum(unlist(df))
          denominator <- length(unlist(df))
          X_mean_mean <- numerator / denominator
          detail$Xmm$setContent(X_mean_mean)
          numerator <- 0
          denominator <- m * n * (m * n - 1)
          for (i in 1:m) {
            for (j in 1:n) {
              d_val <- df[i, j] - X_mean_mean
              numerator <- numerator + d_val * d_val
            }
          }
          Ureprel <- sqrt(numerator / denominator) / X_mean_mean
          detail$Ureprel$setContent(Ureprel)
        } else {
          if (self$options$autow) {

          } else {
            W_i <- as.vector(self$data[, self$options$w])
          }
          
          numerator <- 0
          denominator <- sum(W_i)
          x_i_mean <- rowMeans(df)
          for (i in 1:m) {
            numerator <- numerator + W_i[i] * x_i_mean[i]
          }
          X_mean_mean <- as.numeric(numerator / denominator)
          detail$Xmm$setContent(X_mean_mean)
          numerator <- 0
          denominator <- (m - 1) * sum(W_i)
          for (i in 1:m) {
            d_val <- x_i_mean[i] - X_mean_mean
            numerator <- numerator + W_i[i] * d_val^2
          }
          Ureprel <- as.numeric(sqrt(numerator / denominator) / X_mean_mean)
          detail$Ureprel$setContent(Ureprel)
        }
        detail$Wi$setContent(W_i)
        Ucharrel <- sqrt(Uwcalrel^2 + Ureprel^2 + Uorel^2)
        detail$Ucharrel$setContent(Ucharrel)

        uchar <- Ucharrel * C
        self$results$uchar$setContent(uchar)
      }
    )
  )
}
