# This file is a generated template, your changes will not be overwritten

KruskalWallisHTestForMultipleSamplesFrequencyClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "KruskalWallisHTestForMultipleSamplesFrequencyClass",
    inherit = KruskalWallisHTestForMultipleSamplesFrequencyBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        f <- self$data[, self$options$f]
        a <- as.numeric(self$options$a)
        rf <- rowSums(f)
        N <- sum(f)
        rfr <- rep(seq_along(rf), rf)
        rfrr <- rank(rfr)
        urfrr <- unique(rfrr)
        ri <- apply(f, 2, function(c) sum(c * urfrr))
        ni <- colSums(f)
        rim <- ri / ni
        H <- 12 / N / (N + 1) * sum(ri^2 / ni) - 3 * (N + 1)
        C <- 1 - sum(rf^3 - rf) / (N^3 - N)
        HC <- H / C
        v <- ncol(f) - 1
        P <- 1 - pchisq(HC, v)
        self$results$h0$setTitle(sprintf(
          "$H_0$: %sReject", ifelse(P <= a, "", "Not ")
        ))
        self$results$h0$setContent(
          c(
            sprintf("$H_C=%f$", HC),
            sprintf("$P=%f$", P),
            sprintf("$v=%d$", v)
          )
        )
      }
    )
  )
}
