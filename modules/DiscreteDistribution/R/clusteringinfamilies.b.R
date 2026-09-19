# This file is a generated template, your changes will not be overwritten

ClusteringInFamiliesClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "ClusteringInFamiliesClass",
    inherit = ClusteringInFamiliesBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        A <- self$data[, self$options$A]
        x <- self$data[, self$options$x]
        n <- max(x)
        N <- sum(A) * n
        D <- sum(A * x)
        p <- D / N
        px <- sapply(x, function(v) dbinom(v, n, p))
        T <- sum(A) * px
        v <- length(A) - 2
        chi2 <- sum((A - T)^2 / T)
        P <- 1 - pchisq(chi2, v)
        a <- as.numeric(self$options$a)
        if (P <= a) {
          self$results$h0$setTitle(
            "$H_0$(No Clustering In Families): Reject"
          )
        } else {
          self$results$h0$setTitle(
            "$H_0$(No Clustering In Families): Not Reject"
          )
        }
        self$results$h0$setContent(
          sprintf("$\\chi_2=%f \\\\ v=%d \\\\ P=%f$", chi2, v, P)
        )
      }
    )
  )
}
