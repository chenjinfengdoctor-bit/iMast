# This file is a generated template, your changes will not be overwritten

SpearmanRankCorrelationClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SpearmanRankCorrelationClass",
    inherit = SpearmanRankCorrelationBase,
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
        p <- rank(x)
        q <- rank(y)
        sumd2 <- sum((p - q)^2)
        n <- length(x)
        rs <- 1 - 6 * sumd2 / n / (n^2 - 1)

        tx <- n - length(unique(p))
        ty <- n - length(unique(q))
        Tx <- sum(tx^3 - tx) / 12
        Ty <- sum(ty^3 - ty) / 12
        rsc <- ((n^3 - n) / 6 - Tx - Ty - sumd2) /
          sqrt((n^3 - n) / 6 - 2 * Tx) /
          sqrt((n^3 - n) / 6 - 2 * Ty)
        c <- self$options$correct
        if (c) {
          rs <- rsc
        }
        u <- rs * sqrt(n - 1)
        P <- 2 * (1 - pnorm(u))
        alpha <- self$options$alpha
        content <- c(
          ifelse(P <= alpha, "Reject", "Not Reject"),
          sprintf(texformat("$\\sum d^2=%f$"), sumd2),
          sprintf(texformat("$r_s=1-\\frac{6 \\sum d^2}{n(n^2-1)}=%f$"), rs),
          sprintf(texformat("$T_X=\\sum (t_X^3-t_X)/12=%f$"), Tx),
          sprintf(texformat("$T_Y=\\sum (t_Y^3-t_Y)/12=%f$"), Ty),
          sprintf(texformat(
            "$r_s'=\\frac{[(n^3-n)/6]-(T_X+T_Y)-\\sum d^2}{\\sqrt{[(n_3-n)/6]-2T_X}\\sqrt{[(n_3-n)/6]-2T_Y}}=%f$"
          ), rsc),
          sprintf(
            texformat("$u=r_s%s\\sqrt{n-1}=%f$"),
            ifelse(c, "'", ""), u
          ),
          sprintf(texformat("$P=%f$"), P)
        )
        if (!c) {
          content <- content[-c(4, 5, 6)]
        }
        self$results$h0$setContent(content)
      }
    )
  )
}
