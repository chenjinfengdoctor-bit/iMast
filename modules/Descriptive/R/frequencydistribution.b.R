# This file is a generated template, your changes will not be overwritten
library(ggplot2)
FrequencyDistributionClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "FrequencyDistributionClass",
    inherit = FrequencyDistributionBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        df <- self$data[, self$options$x]
        l <- na.omit(unlist(df))
        range <- max(l) - min(l)
        ng <- self$options$groups
        i <- range / ng
        if (self$options$i > 0) {
          i <- self$options$i
        }
        group <- cut(
          l,
          breaks = seq(min(l), max(l) + i, by = i),
          include.lowest = TRUE,
          ordered_result = TRUE,
          right = FALSE
        )
        labels <- levels(group)
        left_values <- sapply(
          labels,
          function(x) {
            x <- strsplit(x, ",")[[1]][1]
            x <- substr(x, 2, nchar(x))
            as.numeric(x)
          }
        )
        right_values <- sapply(
          labels,
          function(x) {
            x <- strsplit(x, ",")[[1]][2]
            x <- substr(x, 1, nchar(x) - 1)
            as.numeric(x)
          }
        )
        intervals <- data.frame(left = left_values, right = right_values, f = 0, X = 0)
        group_counts <- table(group)
        ft <- self$results$ft
        row_no <- 1
        sum_f <- 0
        sum_X <- 0
        sum_fX <- 0
        sum_fX2 <- 0
        for (gkey in names(group_counts)) {
          lv <- intervals[row_no, "left"]
          rv <- intervals[row_no, "right"]

          group <- paste0(lv, "~")
          if (row_no == length(group_counts)) {
            group <- paste0(group, rv)
          }
          f <- group_counts[gkey]
          intervals[row_no, "f"] <- f
          X <- lv / 2 + rv / 2
          intervals[row_no, "X"] <- X
          fX <- f * X
          fX2 <- fX * X
          ft$addRow(rowKey = row_no, values = list(
            group = group,
            frequency = as.integer(f),
            median = X,
            fX = fX,
            fX2 = fX2
          ))
          sum_f <- sum_f + f
          sum_X <- sum_X + X
          sum_fX <- sum_fX + fX
          sum_fX2 <- sum_fX2 + fX2
          row_no <- row_no + 1
        }
        ft$addRow(rowKey = "total", values = list(
          group = "Total",
          frequency = as.integer(sum_f),
          median = "-",
          fX = sum_fX,
          fX2 = sum_fX2
        ))
        self$results$gfd$setState(intervals)
      },
      .gfd = function(image, ...) {
        plot <- ggplot(image$state, aes(x = as.character(X), y = f)) +
          geom_bar(stat = "identity", fill = "gray", color = "black") +
          labs(x = "Median", y = "Frequency") +
          theme(
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 14)
          )
        print(plot)
        TRUE
      }
    )
  )
}
