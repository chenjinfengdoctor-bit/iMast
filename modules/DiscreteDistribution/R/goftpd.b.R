# This file is a generated template, your changes will not be overwritten

GOFTPDClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "GOFTPDClass",
    inherit = GOFTPDBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        f <- self$data[, self$options$f]
        N <- sum(f)
        x <- self$data[, self$options$x]
        ox <- order(x)
        x <- x[ox]
        f <- f[ox]
        a <- as.numeric(self$options$a)
        fx <- sum(f * x)
        fx2 <- sum(f * x * x)
        u <- fx / N
        s2 <- (fx2 - fx^2 / N) / (N - 1)
        px <- exp(1)^(-u) * (u^x) / factorial(x)
        px[length(px)] <- 1 - sum(px) + px[[length(px)]]

        T <- px * N
        dt <- data.frame(
          X = x, f = f, PX = px, T = T
        )
        small_rows <- dt[dt$T < 5, ]
        merged_row <- data.frame(
          X = paste(small_rows$X, collapse = ","),
          f = sum(small_rows$f),
          PX = sum(small_rows$PX),
          T = sum(small_rows$T)
        )

        dtm <- dt[dt$T >= 5, ]
        if (merged_row$T < 5) {
          last_row <- dtm[nrow(dtm), ]
          last_row$X <- paste0(
            last_row$X, ",", merged_row$X
          )
          last_row$f <- last_row$f + merged_row$f
          last_row$PX <- last_row$PX + merged_row$PX
          last_row$T <- last_row$T + merged_row$T
          dtm[nrow(dtm), ] <- last_row
        } else {
          dtm <- rbind(dtm, merged_row)
        }
        dtm$att <- (dtm$f - dtm$T)^2 / dtm$T
        chi2 <- sum(dtm$att)
        v <- nrow(dtm) - 2
        P <- 1 - pchisq(chi2, v)
        self$results$goft$setTitle(
          sprintf(
            "$H_0$(Follow a Poisson distribution): %sReject",
            ifelse(P <= a, "", "Not ")
          )
        )
        self$results$goft$setContent(
          c(
            sprintf("$\\sum fX=%d$", fx),
            sprintf("$n=%d$", N),
            sprintf("$\\lambda=\\mu=%f$", u),
            sprintf("$S^2=%f$", s2),
            sprintf("$\\chi^2=%f$", chi2),
            sprintf("$v=%d$", v),
            sprintf("$P=%f$",P),
            capture.output(print(dtm))
          )
        )
      }
    )
  )
}
