# This file is a generated template, your changes will not be overwritten

FriedmanMTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "FriedmanMTestClass",
    inherit = FriedmanMTestBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        a <- as.numeric(self$options$a)
        result <- friedman.test(as.matrix(x))
        r <- t(apply(x, 1, rank))
        R <- colSums(r)
        TIES <- tapply(c(r), row(r), table)
        tj <- sum(unlist(lapply(TIES, function(u) u^3 - u)))
        n <- nrow(x)
        g <- ncol(x)
        Rm <- n * (g + 1) / 2
        M <- sum((R - Rm)^2)
        self$results$h0$setTitle(
          sprintf(
            "$H_0$: %sReject",
            ifelse(result$p.value <= a, "", "Not ")
          )
        )
        self$results$h0$setContent(
          c(
            sprintf("$\\chi^2=%f$", result$statistic),
            sprintf("$v=%d$", result$parameter),
            sprintf("$P=%f$", result$p.value)
          )
        )
        R2 <- sum(R^2)
        q <- list()
        MS <- (
          n * g * (g + 1) * (2 * g + 1) / 6 -
            1 / n * R2 - 1 / 12 * tj
        ) / (n - 1) / (g - 1)
        rrank <- rank(R)
        for (i in seq_len(ncol(x) - 1)) {
          for (j in ncol(x):(i + 1)) {
            ni <- colnames(x)[i]
            nj <- colnames(x)[j]
            pair <- paste0(ni, ",", nj)
            qv <- abs(R[i] - R[j]) / sqrt(n * MS)
            vv <- (n - 1) * (g - 1)
            av <- abs(rrank[i] - rrank[j]) + 1
            pv <- 1 - ptukey(qv, av, vv)
            q[[pair]] <- list(
              pair = pair,
              q = qv,
              v = vv,
              a = av,
              p = pv
            )
          }
        }
        self$results$q$setContent(
          c(
            sprintf("$\\{MS\\}_\\{error\\}=%f$", MS),
            sprintf("$\\sum R_i^2=%f$", R2)
          )
        )
        for (qpair in names(q)) {
          self$results$qt$addRow(
            rowKey = qpair,
            values = q[[qpair]]
          )
        }
      }
    )
  )
}
