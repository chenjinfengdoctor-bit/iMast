# This file is a generated template, your changes will not be overwritten

UFHClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "UFHClass",
    inherit = UFHBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        xm <- colSums(x) / nrow(x)
        xmm <- mean(unlist(x))
        texformat <- function(str) {
          gsub("([{}])", "\\1", str)
        }
        self$results$xm$setContent(c(sprintf(
          texformat("$\\overline{\\overline x}=%f$"),
          xmm
        ), print(capture.output(xm))))
        n <- nrow(x)
        N <- nrow(x) * ncol(x)
        self$results$n$setContent(
          as.character(n)
        )
        ssbb <- sum(n * (xm - xmm)^2)
        m <- ncol(x)
        vbb <- m - 1
        msbb <- ssbb / vbb
        value_wb <- 0
        for (i in seq_len(ncol(x))) {
          value_wb <- value_wb + sum((x[, i] - xm[i])^2)
        }
        mswb <- value_wb / (N - m)
        self$results$msbb$setContent(as.character(msbb))
        self$results$mswb$setContent(as.character(mswb))
        ubb <- 0
        if (msbb < mswb) {
          ubb <- sqrt(mswb / n) * (2 / (N - m))^0.25
        } else {
          ubb <- 1 / xmm * sqrt((msbb - mswb) / n)
        }
        self$results$ufh$setContent(
          sprintf(
            texformat(
              "$u_{bb}=\\frac{1}{\\overline x}\\sqrt{\\frac{(MS_{bb}-MS_{wb})}{n}}=%f$"
            ),
            ubb
          )
        )
      }
    )
  )
}
