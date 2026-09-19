# This file is a generated template, your changes will not be overwritten
library(ggplot2)
tDistributionClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "tDistributionClass",
    inherit = tDistributionBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        v_values <- self$data[, self$options$v]
        if (self$options$vinf) {
          v_values <- append(Inf, v_values)
        }
        data_list <- lapply(v_values, function(v) {
          data.frame(
            x = seq(-5, 5, length.out = 100), # 定义x轴范围
            y = dt(seq(-5, 5, length.out = 100), df = v), # 计算t分布的y值
            v = as.factor(v) # 用不同自由度的标签区分
          )
        })
        data <- do.call(rbind, data_list)
        self$results$gtd$setState(data)
      },
      .gtd = function(image, ...) {
        plot <- ggplot(image$state, aes(x = x, y = y, color = v)) +
          geom_line(size = 1) +
          labs(x = "t", y = "f(t)") +
          theme_minimal() +
          theme(
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.line = element_blank(),
            panel.grid = element_blank()
          ) +
          geom_hline(yintercept = 0, color = "black", size = 0.5) + # 中心水平线
          geom_vline(xintercept = 0, color = "black", size = 0.5) # 中心垂直线
        print(plot)
        TRUE
      }
    )
  )
}
