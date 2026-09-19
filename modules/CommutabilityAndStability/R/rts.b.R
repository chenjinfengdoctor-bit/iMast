# This file is a generated template, your changes will not be overwritten

RTSClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "RTSClass",
    inherit = RTSBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        y <- self$data[, self$options$y]
        xm <- mean(x)
        t <- length(x)
        n <- length(x)
        ymm <- mean(unlist(y))
        b1 <- sum(apply(y, 1, function(r) {
          (mean(r) - ymm)
        }) * (x - xm)) / sum((x - xm)^2)
        b0 <- ymm - b1 * xm
        s <- sqrt(sum((rowMeans(y) - b0 - b1 * x)^2) / (n - 2))
        sb1 <- s / sqrt(sum((x - xm)^2))
        ust1 <- t * sb1
        texformat <- function(str) {
          gsub("([{}])", "\\1", str)
        }
        self$results$rts$setContent(sprintf(
          texformat("$u_{st6}=t \\times s(b_1)=%f$"), ust1
        ))
        self$results$t$setContent(as.character(t))
        self$results$sb1$setContent(
          sprintf(
            texformat(
              "$s(b_1)=\\frac{s}{\\sqrt{\\sum_{i=1}^n{(x_i-\\overline x)^2}}}=%f$"
            ), sb1
          )
        )
        self$results$b1$setContent(
          sprintf(
            texformat(
              "$b_1=\\frac{\\sum_{i=1}^n(x_i-\\overline x)(\\overline y-\\overline{\\overline y})}{\\sum_{i=1}^n (x_i-\\overline x)^2}=%f$"
            ),
            b1
          )
        )
        self$results$s$setContent(
          sprintf(
            texformat(
              "$s=\\sqrt{\\frac{\\sum_{i=1}^n(\\overline y-b_0-b_1 x_i)^2}{n-2}}=%f$"
            ),
            s
          )
        )
        self$results$xi$setContent(
          x
        )
        self$results$xm$setContent(
          sprintf("$\\overline x=%f$", xm)
        )
      }
    )
  )
}
