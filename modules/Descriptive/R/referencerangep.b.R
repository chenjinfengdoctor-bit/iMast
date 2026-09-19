# This file is a generated template, your changes will not be overwritten

ReferenceRangePClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "ReferenceRangePClass",
    inherit = ReferenceRangePBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        l <- self$data[, self$options$l]
        i <- list()
        rr <- as.integer(self$options$rr)
        side <- self$options$side
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
        px <- 0
        if (side == "os") {
          pi <- percentile_groups[[rr]]
          sum_fl <- 0
          if (pi > 1) {
            sum_fl <- cum[[pi - 1]]
          }
          gt_px <- l[[pi]] + i[[pi]] / f[[pi]] *
            (total_count * rr * 0.01 - sum_fl)
          rr <- 100 - rr
          pi <- percentile_groups[[rr]]
          sum_fl <- 0
          if (pi > 1) {
            sum_fl <- cum[[pi - 1]]
          }
          lt_px <- l[[pi]] + i[[pi]] / f[[pi]] *
            (total_count * rr * 0.01 - sum_fl)
          self$results$text$setTitle(
            paste0(
              ">", gt_px, " or <", lt_px
            )
          )
        } else {
          rr <- as.integer(rr / 2)
          pi <- percentile_groups[[rr]]
          sum_fl <- 0
          if (pi > 1) {
            sum_fl <- cum[[pi - 1]]
          }
          gt_px <- l[[pi]] + i[[pi]] / f[[pi]] *
            (total_count * rr * 0.01 - sum_fl)
          rr <- 100 - rr
          pi <- percentile_groups[[rr]]
          sum_fl <- 0
          if (pi > 1) {
            sum_fl <- cum[[pi - 1]]
          }
          lt_px <- l[[pi]] + i[[pi]] / f[[pi]] *
            (total_count * rr * 0.01 - sum_fl)
          self$results$text$setTitle(
            paste0(
              gt_px, "~", lt_px
            )
          )
        }
      }
    )
  )
}
