# This file is a generated template, your changes will not be overwritten

DynamicSeriesClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "DynamicSeriesClass",
    inherit = DynamicSeriesBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        y <- self$data[, self$options$y]
        cag <- numeric(length(x))
        gag <- numeric(length(x))
        bpdr <- numeric(length(x))
        sdr <- numeric(length(x))
        bpgr <- numeric(length(x))
        sgr <- numeric(length(x))
        cag[1] <- 0
        gag[1] <- 0
        bpdr[1] <- 100
        sdr[1] <- 100
        bpgr[1] <- 0
        sgr[1] <- 0
        for (i in 2:length(x)) {
          cag[i] <- y[[i]] - y[[1]]
          gag[i] <- y[[i]] - y[[i - 1]]
          bpdr[i] <- y[[i]] / y[[1]] * 100
          sdr[i] <- y[[i]] / y[[i - 1]] * 100
          bpgr[i] <- bpdr[i] - 100
          sgr[i] <- sdr[i] - 100
        }
        for (i in seq_along(x)) {
          self$results$dst$addRow(
            rowKey = i,
            values = list(
              x = x[i],
              y = y[i],
              cag = cag[i],
              gag = gag[i],
              bpdr = bpdr[i],
              sdr = sdr[i],
              bpgr = bpgr[i],
              sgr = sgr[i]
            )
          )
        }
        if (self$options$f > x[[length(x)]]) {
          m_sdr <- (y[[length(y)]] / y[[1]])^(1 / (length(x) - 1))
          fv <- m_sdr^(self$options$f - x[[1]]) * y[[1]]
          self$results$dst$addRow(
            rowKey = -1,
            values = list(
              x = self$options$f,
              y = fv
            )
          )
        }
      }
    )
  )
}
