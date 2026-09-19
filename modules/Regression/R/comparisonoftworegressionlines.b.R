# This file is a generated template, your changes will not be overwritten

ComparisonOfTwoRegressionLinesClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "ComparisonOfTwoRegressionLinesClass",
    inherit = ComparisonOfTwoRegressionLinesBase,
    private = list(
      .run = function() {
        texformat <- function(str) {
          gsub("([{}])", "\\1", str)
        }
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x1 <- na.omit(self$data[, self$options$x1])
        x2 <- na.omit(self$data[, self$options$x2])

        n1 <- length(x1)
        n2 <- length(x2)
        y1 <- na.omit(self$data[, self$options$y1])
        y2 <- na.omit(self$data[, self$options$y2])
        lxy1 <- sum(x1 * y1) - sum(x1) * sum(y1) / n1
        lxx1 <- sum(x1^2) - sum(x1)^2 / n1
        lyy1 <- sum(y1^2) - sum(y1)^2 / n1

        lxy2 <- sum(x2 * y2) - sum(x2) * sum(y2) / n2
        lxx2 <- sum(x2^2) - sum(x2)^2 / n2
        lyy2 <- sum(y2^2) - sum(y2)^2 / n2
        b1 <- lxy1 / lxx1
        b2 <- lxy2 / lxx2
        bc <- (lxy1 + lxy2) / (lxx1 + lxx2)
        ss_common <- lyy1 - bc * lxy1 + lyy2 - bc * lxy2
        ss_wg <- lyy1 - lxy1^2 / lxx1 + lyy2 - lxy2^2 / lxx2
        F <- (ss_common - ss_wg) / ss_wg * (n1 + n2 - 4)
        v1 <- 1
        v2 <- n1 + n2 - 4
        pf <- 1 - pf(F, v1, v2)
        a <- self$options$alpha
        self$results$cf$setContent(
          c(
            sprintf(
              "$H_0: \\beta_1 = \\beta_2$ (parallel): %sReject",
              ifelse(pf <= a, "", "Not ")
            ),
            sprintf(texformat("$F=%f$"), F),
            sprintf(texformat("$v_1=%d,v_2=%d$"), v1, v2),
            sprintf(texformat("$P=%f$"), pf),
            sprintf(texformat("$b_c=%f$"), bc)
          )
        )
        ss_res1 <- lyy1 - lxy1^2 / lxx1
        ss_res2 <- lyy2 - lxy2^2 / lxx2
        S <- sqrt((ss_res1 + ss_res2) / (n1 + n2 - 4))
        Sb1b2 <- S * sqrt(1 / lxx1 + 1 / lxx2)
        t <- (b1 - b2) / Sb1b2
        v <- n1 + n2 - 4
        pt <- 2 - 2 * pt(abs(t), v)
        self$results$ct$setContent(
          c(
            sprintf(
              "$H_0: \\beta_1 = \\beta_2$ (parallel): %sReject",
              ifelse(pt <= a, "", "Not ")
            ),
            sprintf(texformat("$S=%f$"), S),
            sprintf(texformat("$S_{b_1-b_2}=%f$"), Sb1b2),
            sprintf(texformat("$t=%f$"), t),
            sprintf(texformat("$v=%d$"), v),
            sprintf(texformat("$P=%f$"), pt)
          )
        )
        x <- c(x1, x2)
        y <- c(y1, y2)
        model <- lm(y ~ x, data = data.frame(x = x, y = y))
        ss_total <- sum(resid(model)^2)
        iF <- (ss_total - ss_common) / ss_common * (n1 + n2 - 3)
        ipf <- 1 - pf(iF, 1, n1 + n2 - 3)
        self$results$iF$setContent(
          c(
            sprintf(
              "$H_0$: (equal intercepts): %sReject",
              ifelse(ipf <= a, "", "Not ")
            ),
            sprintf(texformat("$SS_{total}=%f$"), ss_total),
            sprintf(texformat("$SS_{common}=%f$"), ss_common),
            sprintf(texformat("$F=%f$"), iF),
            sprintf(texformat("$v_1=%d,v_2=%d$"), 1, n1 + n2 - 3),
            sprintf(texformat("$P=%f$"), ipf)
          )
        )
        ac1 <- mean(y1) - bc * mean(x1)
        ac2 <- mean(y2) - bc * mean(x2)
        sc <- sqrt(ss_common / (n1 + n2 - 3))
        sac1ac2 <- sc * sqrt(
          1 / n1 + 1 / n2 + (mean(x1) - mean(x2))^2 / (lxx1 + lxx2)
        )
        iT <- (ac1 - ac2) / sac1ac2
        iv <- n1 + n2 - 3
        ipt <- 2 - 2 * pt(abs(iT), iv)
        self$results$iT$setContent(
          c(
            sprintf(
              "$H_0$: (equal intercepts): %sReject",
              ifelse(ipt <= a, "", "Not ")
            ),
            sprintf(texformat("$a_{c_1}-a_{c_2}=%f$"), ac1 - ac2),
            sprintf(texformat("$S_c=%f$"), sc),
            sprintf(texformat("$S_{a_{c_1}-a_{c_2}}=%f$"), sac1ac2),
            sprintf(texformat("$t=%f$"), iT),
            sprintf(texformat("$v=%d$"), iv),
            sprintf(texformat("$P=%f$"), ipt)
          )
        )
      }
    )
  )
}
