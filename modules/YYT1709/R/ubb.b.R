# This file is a generated template, your changes will not be overwritten

ubbClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "ubbClass",
    inherit = ubbBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        table <- self$results$table
        detail <- self$results$detail$items
        cols <- self$options$x
        df <- self$data[, cols]
        m <- nrow(df)
        x_i_mean <- rowMeans(df, na.rm = TRUE)
        detail$xim$setContent(x_i_mean)
        x_mean_mean <- mean(unlist(df), na.rm = TRUE)
        detail$xmm$setContent(x_mean_mean)
        n_i <- apply(df, 1, function(x) sum(!is.na(x)))
        detail$ni$setContent(n_i)
        N <- sum(n_i)
        detail$N$setContent(N)
        SS_bb <- sum(n_i * (x_i_mean - x_mean_mean)^2)
        detail$SSbb$setContent(SS_bb)
        SS_wb <- sum(apply(df, 1, function(x) sum((x - mean(x))^2)))
        detail$SSwb$setContent(SS_wb)
        v_bb <- m - 1
        detail$vbb$setContent(v_bb)
        v_wb <- N - m
        detail$vwb$setContent(v_wb)
        MS_bb <- SS_bb / v_bb
        detail$MSbb$setContent(MS_bb)
        MS_wb <- SS_wb / v_wb
        detail$MSwb$setContent(MS_wb)
        f <- MS_bb / MS_wb
        detail$F$setContent(f)
        F005vbbvwb <- qf(1 - 0.05, df1 = v_bb, df2 = v_wb)
        detail$F005vbbvwb$setContent(F005vbbvwb)
        uniformity <- ""
        remake <- "No need to re-prepare"
        val <- sqrt((MS_bb - MS_wb) / n_i[[1]])
        u_d <- 0
        u_bb <- val
        s_t <- sqrt(MS_wb)
        if (f >= F005vbbvwb) {
          uniformity <- "Poor homogeneity"
          if (val <= u_d / 3) {
          } else {
            remake <- "Needs re-preparation"
          }
        } else if (1 < f && f < F005vbbvwb) {
          uniformity <- "Good homogeneity"
        } else if (f <= 1 || sqrt(MS_wb) > u_d / 3) {
          uniformity <- "Poor repeatability of homogeneity test method"
          u_bb <- sqrt(MS_wb / sum(n_i)) * sqrt(sqrt(2 / v_wb))
        }
        self$results$ubb$setContent(u_bb)
        self$results$result$setContent(paste(uniformity, remake, sep = ","))
        table$setRow(
          rowNo = 1,
          value = list(
            source = "Between groups",
            SS = SS_bb,
            v = v_bb,
            MS = MS_bb,
            F = f,
            F005vbbvwb = F005vbbvwb
          )
        )
        table$setRow(
          rowNo = 2,
          value = list(
            source = "Within groups",
            SS = SS_wb,
            v = v_wb,
            MS = MS_wb,
            F = f,
            F005vbbvwb = F005vbbvwb
          )
        )
      }
    )
  )
}
