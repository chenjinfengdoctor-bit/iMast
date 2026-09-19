# This file is a generated template, your changes will not be overwritten

LinearRegressionClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "LinearRegressionClass",
    inherit = LinearRegressionBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        y <- self$data[, self$options$y]
        xm <- mean(x)
        ym <- mean(y)
        n <- length(x)
        lxy <- sum(x * y) - sum(x) * sum(y) / n
        lxx <- sum(x^2) - sum(x)^2 / n
        lyy <- sum(y^2) - sum(y)^2 / n
        b <- lxy / lxx
        a <- ym - b * xm
        alpha <- as.numeric(self$options$a)
        texformat <- function(str) {
          gsub("([{}])", "\\1", str)
        }
        self$results$result$setTitle(
          sprintf(texformat("$\\hat Y = a + bX = %f + %fX$"), a, b)
        )
        self$results$result$setContent(
          c(
            sprintf(texformat("$\\overline X=%f$"), xm),
            sprintf(texformat("$\\overline Y=%f$"), ym),
            sprintf(
              texformat("$l_{XX}=\\sum{X^2}-\\frac{(\\sum X)^2}{n}=%f$"), lxx
            ),
            sprintf(
              texformat("$l_{YY}=\\sum{Y^2}-\\frac{(\\sum Y)^2}{n}=%f$"), lyy
            ),
            sprintf(
              texformat("$l_{XY}=\\sum{XY}-\\frac{(\\sum X)(\\sum Y)}{n}=%f$"),
              lxy
            ),
            sprintf(texformat("$b=\\frac{l_{XY}}{l_{XX}}=%f$"), b),
            sprintf(texformat("$a=\\overline Y - b \\overline X=%f$"), a)
          )
        )
        lr <- function(x) a + b * x
        ss_regression <- sum((lr(x) - ym)^2)
        ss_residual <- sum((y - lr(x))^2)
        ss_total <- ss_regression + ss_residual
        v_regression <- 1
        v_residual <- n - 2
        v_total <- n - 1
        ms_regression <- ss_regression / v_regression
        ms_residual <- ss_residual / v_residual
        F <- ms_regression / ms_residual
        Pf <- df(F, v_regression, v_residual)
        self$results$anovat$addRow(
          rowKey = "Total Variation",
          values = list(
            vs = "Total Variation",
            df = v_total,
            ss = ss_total
          )
        )
        self$results$anovat$addRow(
          rowKey = "Regression",
          values = list(
            vs = "Regression",
            df = v_regression,
            ss = ss_regression,
            ms = ms_regression,
            F = F,
            P = Pf
          )
        )
        self$results$anovat$addRow(
          rowKey = "Residual",
          values = list(
            vs = "Residual",
            df = v_residual,
            ss = ss_residual,
            ms = ms_residual
          )
        )
        self$results$anova$setContent(
          c(
            sprintf(texformat("$SS_{regression}=\\sum(\\hat Y - \\overline Y)^2=%f$"), ss_regression),
            sprintf(texformat("$SS_{residual}=\\sum(Y - \\hat Y)^2=%f$"), ss_residual),
            sprintf(texformat("$SS_{total}=SS_{regression}+SS_{residual}=\\sum(Y - \\overline Y)^2=%f$"), ss_total),
            sprintf(texformat("$v_{total}=n-1=%d$"), v_total),
            sprintf(texformat("$v_{regression}=%d$"), v_regression),
            sprintf(texformat("$v_{residual}=n-2=%d$"), v_residual),
            sprintf(texformat("$F=\\frac{SS_{regression}/v_{regression}}{SS_{residual}/v_{residual}}=%f$"), F),
            sprintf(texformat("$P_{F,(%d,%d)}=%f$"), v_regression, v_residual, Pf)
          )
        )
        self$results$anova$setTitle(
          paste0(self$results$anova$title, sprintf(texformat(": $H_0(\\beta = 0)$: %sReject"), ifelse(Pf <= alpha, "", "Not ")))
        )
        syx <- sqrt(ss_residual / (n - 2))
        sb <- syx / sqrt(lxx)
        t <- (b - 0) / sb
        Pt <- 2 - 2 * pt(t, v_residual)
        self$results$ttest$setContent(
          c(
            sprintf(texformat("$S_{Y\\cdot X}=\\sqrt{\\frac{SS_{residual}}{n-2}}=%f$"), syx),
            sprintf(texformat("$S_b=\\frac{S_{Y\\cdot X}}{\\sqrt{l_{XX}}}=%f$"), sb),
            sprintf(texformat("$t=\\frac{b-0}{S_b}=\\sqrt{F}=%f$"), t),
            sprintf(texformat("$v=%d$"), v_residual),
            sprintf(texformat("$P_{t,v}=%f$"), Pt)
          )
        )
        self$results$ttest$setTitle(
          paste0(
            self$results$ttest$title,
            sprintf(texformat(": $H_0(\\beta = 0)$: %sReject"), ifelse(Pt <= alpha, "", "Not "))
          )
        )
        cld <- qt(1 - alpha / 2, v_residual)
        cll <- b - cld * sb
        clr <- b + cld * sb
        self$results$cl$setContent(
          c(
            sprintf(
              texformat(
                "$\\beta \\in b\\pm t_{\\alpha/2,v}S_b=(%f,%f)$"
              ), cll, clr
            )
          )
        )
        x0 <- self$options$x0
        y0 <- lr(x0)
        syh0 <- syx * sqrt(1 / n + (x0 - xm)^2 / sum((x - xm)^2))
        pml <- y0 - cld * syh0
        pmr <- y0 + cld * syh0
        self$results$pm$setContent(
          c(
            sprintf(texformat(
              "$S_{\\hat{Y}_0}=S_{Y \\cdot X} \\sqrt{\\frac{1}{n}+\\frac{(X_0-\\overline X)^2}{\\sum (X-\\overline X)^2}}=%f$"
            ), syh0),
            sprintf(texformat(
              "$\\mu_{Y|X_0} \\in \\hat{Y}_0 \\pm t_{\\alpha/2,v}S_{\\hat{Y}_0}=(%f,%f)$"
            ), pml, pmr)
          )
        )
        sy0 <- syx * sqrt(1 + 1 / n + (x0 - xm)^2 / sum((x - xm)^2))
        pil <- lr(x0) - cld * sy0
        pir <- lr(x0) + cld * sy0
        self$results$iy$setContent(
          c(
            sprintf(texformat(
              "$S_{Y_0}=S_{Y \\cdot X}\\sqrt{1+\\frac{1}{n}+\\frac{(X_0-\\overline X)^2}{\\sum (X-\\overline X)^2}}=%f$"
            ), sy0),
            sprintf(texformat(
              "$\\hat Y_0 \\pm t_{\\alpha/2,v}S_{Y_0}=(%f,%f)$"
            ), pil, pir)
          )
        )
      }
    )
  )
}
