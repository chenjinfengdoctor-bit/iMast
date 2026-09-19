# This file is a generated template, your changes will not be overwritten
library(car)
LeveneTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "LeveneTestClass",
    inherit = LeveneTestBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        x <- as.data.frame(apply(x, 2, function(x) na.omit(x)))
        g <- ncol(x)
        a <- as.numeric(self$options$a)
        xl <- stack(x)
        N <- nrow(xl)
        colnames(xl) <- c("Value", "Name")
        lt <- NULL
        if (self$options$center == "mean") {
          lt <- leveneTest(
            Value ~ Name,
            data = xl,
            center = "mean"
          )
        } else if (self$options$center == "median") {
          lt <- leveneTest(
            Value ~ Name,
            data = xl,
            center = "median"
          )
        } else if (self$options$center == "trimmed") {
          lt <- leveneTest(
            Value ~ Name,
            data = xl,
            center = "mean",
            trim = 0.1
          )
        }
        F <- lt[["F value"]][1]
        Pr <- lt[["Pr(>F)"]][1]
        Fa <- df(1 - a, g - 1, N - g)
        h0 <- "$H_0(\\sigma_1^2=\\sigma_2^2=\\cdots=\\sigma_g^2=\\sigma^2)$"
        if (Pr > a) {
          self$results$conclusion$setTitle(
            paste0(h0, ": Not Reject")
          )
        } else {
          self$results$conclusion$setTitle(
            paste0(h0, ": Reject")
          )
        }
        self$results$conclusion$setContent(
          sprintf(
            "$F=%.2f \\\\ F_\\{%.2f,(%d,%d)\\}=%.2f \\\\ P=%.4f$",
            F, a, g - 1, N - g, Fa, Pr
          )
        )
      }
    )
  )
}
