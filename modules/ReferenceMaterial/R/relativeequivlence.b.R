# This file is a generated template, your changes will not be overwritten

RelativeEquivlenceClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "RelativeEquivlenceClass",
    inherit = RelativeEquivlenceBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        u <- function(df) {
          n <- nrow(df)
          m <- ncol(df)
          return(sd(as.matrix(df)) / sqrt(n))
          # v <- unlist(df, use.names = FALSE)
          # u_i <- sapply(df, function(col) sd(col) / sqrt(m))
          # u_combined <- sqrt(mean(u_i^2))
          # return(u_combined)
        }

        xcm1 <- self$options$xcm1
        xcm2 <- self$options$xcm2
        uxcm2 <- self$options$uxcm2
        k <- self$options$k
        ycm1 <- self$data[, self$options$ycm1]
        ycm2 <- self$data[, self$options$ycm2]
        ycm1.m <- mean(as.matrix(ycm1))
        ycm2.m <- mean(as.matrix(ycm2))
        xcm2t <- ycm2.m * xcm1 / ycm1.m
        uycm1 <- u(ycm1)
        uycm1.rel <- uycm1 / ycm1.m
        uycm2 <- u(ycm2)
        uycm2.rel <- uycm2 / ycm2.m
        uxcm2.rel <- uxcm2 / xcm2
        uxcm2t <- xcm2t * sqrt(uycm1.rel^2 + uycm2.rel^2 + uxcm2.rel^2)
        D <- xcm2 - xcm2t
        uD <- sqrt(uxcm2^2 + uxcm2t^2)
        equivalent <- abs(D) <= k * uD
        self$results$text$setContent(
          c(
            tprintf("Certified values of reference materials $(x_{cm1})$: %s", sp(xcm1)),
            tprintf("Certified values of control materials $(x_{cm2})$: %s", sp(xcm2)),
            tprintf("Repetitions $(n)$: %d", nrow(ycm1)),
            tprintf("Number of samples $(m)$: %d", ncol(ycm1)),
            tprintf("Grand mean of measurements from reference materials $(y_{cm1})$:\n $y_{cm1}=\\sum_{i=1}^{n}\\sum_{j=1}^{m} y_{cm1(i,j)}/(nm)=$ %s", sp(ycm1.m)),
            tprintf("Grand mean of measurements from control materials $(y_{cm2})$:\n $y_{cm2}=\\sum_{i=1}^{n}\\sum_{j=1}^{m} y_{cm2(i,j)}/(nm)=$ %s", sp(ycm2.m)),
            tprintf("Measurement result correction value from control materials $(x'_{cm2})$:\n $x'_{cm2} = (y_{cm2} \\cdot x_{cm1})/y_{cm1}=$ %s", sp(xcm2t)),
            tprintf("Uncertainty of measurements from reference materials $(u(y_{cm1}))$:\n $u(y_{cm1}) = \\sqrt{\\sum_{i=1}^m\\sum_{j=1}^n(y_{cm1(i,j)}-y_{cm1})^2/(mn-1)}/\\sqrt{n}=$ %s", sp(uycm1)),
            tprintf("Uncertainty of measurements from control materials $(u(y_{cm2}))$:\n $u(y_{cm2}) = \\sqrt{\\sum_{i=1}^m\\sum_{j=1}^n(y_{cm2(i,j)}-y_{cm2})^2/(mn-1)}/\\sqrt{n}=$ %s", sp(uycm2)),
            tprintf("Uncentainty of certified values from control materials $(u(x_{cm2}))$: %s", sp(uxcm2)),
            tprintf("Relative uncertainty of measurements from reference materials $(u_{rel}(y_{cm1}))$:\n $u_{rel}(y_{cm1}) = u(y_{cm1})/y_{cm1} = $ %s", sp(uycm1.rel)),
            tprintf("Relative uncertainty of measurements from control materials $(u_{rel}(y_{cm2}))$:\n $u_{rel}(y_{cm2}) = u(y_{cm2})/y_{cm2} = $ %s", sp(uycm2.rel)),
            tprintf("Relative uncertainty of certified values from control materials $(u_{rel}(x_{cm2}))$:\n $u_{rel}(x_{cm2}) = u(x_{cm2})/x_{cm2} = $ %s", sp(uxcm2.rel)),
            tprintf("Uncertainty of measurement result correction value from control materials $(u(x'_{cm2}))$:\n $u(x'_{cm2}) = x'_{cm2}\\sqrt{u^2_{rel}(y_{cm1})+u^2_{rel}(y_{cm2})+u^2_{rel}(x_{cm2})}=$ %s", sp(uxcm2t)),
            tprintf("Bias of the certified value and the measurement result correction value from control materials $(D)$:\n $D = x_{cm2}-x'_{cm2} = $ %s", sp(D)),
            tprintf("Uncertainty of bias of the certified value and the measurement result correction value from control materials $(u(D))$:\n $u(D) = \\sqrt{u^2(x_{cm2})+u^2(x'_{cm2})} = $ %s", sp(uD)),
            tprintf("Coverage factor $(k)$: %d", k),
            tprintf("Expanded uncertainty of bias $(ku(D))$: %s", sp(k * uD))
          )
        )
        self$results$t$addRow(
          rowKey = "xcm1",
          values = list(
            p1 = tprintf("$x_{cm1}$"),
            v1 = xcm1,
            p2 = tprintf("$x_{cm2}$"),
            v2 = xcm2
          )
        )
        self$results$t$addRow(
          rowKey = "ycm1",
          values = list(
            p1 = tprintf("$y_{cm1}$"),
            v1 = ycm1.m,
            p2 = tprintf("$y_{cm2}$"),
            v2 = ycm2.m
          )
        )
        self$results$t$addRow(
          rowKey = "xcm2t",
          values = list(
            p1 = tprintf("$x'_{cm2}$"),
            v1 = xcm2t,
            p2 = tprintf("$u(y_{cm1})$"),
            v2 = uycm1
          )
        )
        self$results$t$addRow(
          rowKey = "uycm2",
          values = list(
            p1 = tprintf("$u(y_{cm2})$"),
            v1 = uycm2,
            p2 = tprintf("$u(x_{cm2})$"),
            v2 = uxcm2
          )
        )

        self$results$t$addRow(
          rowKey = "uxcm2t",
          values = list(
            p1 = tprintf("$u(x'_{cm2})$"),
            v1 = uxcm2t,
            p2 = tprintf("$D$"),
            v2 = D
          )
        )
        self$results$t$addRow(
          rowKey = "uD",
          values = list(
            p1 = tprintf("$u(D)$"),
            v1 = uD,
            p2 = tprintf("$k$"),
            v2 = k
          )
        )
        self$results$t$addRow(
          rowKey = "eq",
          values = list(
            p1 = tprintf("$ku(D)$"),
            v1 = k * uD,
            p2 = tprintf("Equivalent"),
            v2 = ifelse(equivalent, "Yes", "No")
          )
        )
        self$results$c$setContent(
          # tprintf(
          #   "Since $|D| = %s %s ku(D) = %s$, control material values are%s equivalent with reference material at %s%% inclusion probability (k=%d).",
          #   sp(D), ifelse(equivalent, "\\le", "\\gt"), sp(k * uD), ifelse(equivalent, "", " not"), "95", k
          # )
          tprintf(
            "Since $|D| = %s %s ku(D) = %s$, the control material values are%s equivalent, demonstrating its traceability is%s reliable.",
            sp(D), ifelse(equivalent, "\\le", "\\gt"), sp(k * uD), ifelse(equivalent, "", " not"), ifelse(equivalent, "", " not")
          )
        )
      }
    )
  )
}
