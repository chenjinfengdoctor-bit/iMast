# This file is a generated template, your changes will not be overwritten

CoefficientOfLineCorrelationClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "CoefficientOfLineCorrelationClass",
    inherit = CoefficientOfLineCorrelationBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        y <- self$data[, self$options$y]
        texformat <- function(str) {
          gsub("([{}])", "\\1", str)
        }
        mx <- mean(x)
        my <- mean(y)
        r <- sum((x - mx) * (y - my)) /
          sqrt(sum((x - mx)^2)) / sqrt(sum((y - my)^2))
        self$results$r$setContent(
          sprintf(
            texformat(
              "$r=\\frac{\\sum (X-\\overline X)(Y-\\overline Y)}{\\sqrt{\\sum (X-\\overline X)^2}\\sqrt{\\sum (Y-\\overline Y)^2}}=\\frac{l_{XY}}{\\sqrt{l_{XX}l_{YY}}}=%f$"
            ),
            r
          )
        )
        alpha <- self$options$alpha
        n <- length(x)
        t <- r / sqrt((1 - r^2) / (n - 2))
        p <- 2 * (1 - pt(t, n - 2))
        self$results$h0$setContent(
          c(
            ifelse(p <= alpha, "Reject", "Not Reject"),
            sprintf(texformat(
              "$t=\\frac{r}{\\sqrt{\\frac{1-r^2}{n-2}}}=%f$"
            ), t),
            sprintf(texformat("$v=n-2=%d$"), n - 2),
            sprintf(
              texformat("$P = %f %s \\alpha$"),
              p, ifelse(p <= alpha, "\\lt", "\\gt")
            )
          )
        )
        z <- 1 / 2 * log((1 + r) / (1 - r))
        zci <- c(
          z - qnorm(1 - alpha / 2) / sqrt(n - 3),
          z + qnorm(1 - alpha / 2) / sqrt(n - 3)
        )
        ci <- tanh(zci)
        self$results$ci$setContent(c(
          sprintf(texformat(
            "$z = \\frac{1}{2}\\ln\\frac{(1+r)}{(1-r)}=%f$"
          ), z),
          sprintf(texformat(
            "$ z \\pm u_{\\alpha/2}/\\sqrt{n-3} = (%f,%f)$"
          ), zci[[1]], zci[[2]]),
          sprintf(texformat(
            "$r = \\tanh z \\in (%f,%f)$"
          ), ci[[1]], ci[[2]])
        ))
        R2 <- r^2
        F <- R2 / (1 - R2) * (n - 2)
        self$results$r2$setContent(
          c(
            sprintf(texformat("$R^2=r^2=%f$"), R2),
            sprintf(texformat(
              "$F=\\frac{R^2}{(1-R^2)/(n-2)}=%f$"
            ), F),
            sprintf(texformat(
              "$v_1=%d, v_2=%d$"
            ), 1, n - 2)
          )
        )
      }
    )
  )
}
