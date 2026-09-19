# This file is a generated template, your changes will not be overwritten

PercentileFrequencyClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "PercentileFrequencyClass",
    inherit = PercentileFrequencyBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        l <- self$data[, self$options$l]
        i <- list()
        if (is.null(self$options$i)) {
          i <- sapply(2:length(l), function(ii) {
            l[[ii]] - l[[ii - 1]]
          })
          i <- append(i, i[[length(i)]])
        } else {
          i <- self$data[, self$options$i]
        }
        f <- self$data[, self$options$f]
        total_count <- sum(f)
        cum <- cumsum(f)
        percentile_groups <- sapply(1:99, function(n) {
          percentile_position <- total_count * n / 100
          group_index <- which(cum > percentile_position)[1]
          group_index
        })
        pi <- 0
        p25 <- 0
        p50 <- 0
        p75 <- 0
        for (ii in 1:99) {
          pi <- percentile_groups[[ii]]
          sum_fl <- 0
          if (pi > 1) {
            sum_fl <- cum[[pi - 1]]
          }
          px <- l[[pi]] + i[[pi]] / f[[pi]] * (total_count * ii * 0.01 - sum_fl)
          if (ii == 25) {
            p25 <- px
          } else if (ii == 75) {
            p75 <- px
          } else if (ii == 50) {
            p50 <- px
          }
          if (self$options$X == ii) {
            format <- paste0(
              "$P_\\{",
              ii,
              "\\}=L_X+\\frac\\{i_X\\}\\{f_X\\}(nX\\%-\\sum f_L)=",
              px,
              "$"
            )
            self$results$sp$setTitle(format)
          }
          self$results$pt$addRow(
            rowKey = ii,
            values = list(
              x = ii,
              px = px
            )
          )
        }
        if (self$options$qr) {
          qr <- p75 - p25
          self$results$qr$setTitle(
            paste0(
              "$QR=P_\\{75\\}-P_\\{25\\}=",
              qr,
              "$"
            )
          )
        }
      }
    )
  )
}
