# This file is a generated template, your changes will not be overwritten

UCVClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "UCVClass",
    inherit = UCVBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        a <- as.numeric(self$options$a)
        xl <- unlist(x)
        n <- length(xl)
        xm <- sum(xl) / n
        ureprel <- 1 / xm * sqrt(1 / n / (n - 1) * sum((x - xm)^2))
        texformat <- function(str) {
          gsub("([{}])", "\\1", str)
        }
        self$results$empu$setContent(
          c(
            sprintf("$\\overline x=%f$", xm),
            sprintf(
              texformat("$u_{rep.rel}=\\frac{1}{\\overline x}\\sqrt{\\frac{1}{n(n-1)}\\sum_{i=1}^n{(x_i-\\overline x)^2}}=%f$"),
              ureprel
            )
          )
        )
        uweighrel <- abs(self$options$mpem) / self$options$m / sqrt(3)
        self$results$ewu$setContent(
          c(
            sprintf(
              texformat(
                "$u_{weigh.rel}=\\frac{|MPE_m|}{m \\times \\sqrt{3}}=%f$"
              ),
              uweighrel
            )
          )
        )
        uwcal <- self$options$uwcal
        k <- self$options$k
        cwcal <- self$options$cwcal
        uwcalrel <- uwcal / k / cwcal
        self$results$eiscu$setContent(
          c(
            sprintf(
              texformat("$u_{wcal.rel}=\\frac{u_{wcal}}{k \\times C_{wcal}}=%f$"),
              uwcalrel
            )
          )
        )
        ucharrel <- sqrt(ureprel^2 + uwcalrel^2 + uweighrel^2)
        uchar <- ucharrel * self$options$c
        self$results$ucv$setContent(
          c(
            sprintf(texformat(
              "$u_{char.rel}=\\sqrt{u_{rep.rel}^2+u_{wcal.rel}^2+u_{weigh.rel}^2}=%f$"
            ), ucharrel),
            sprintf(texformat(
              "$u_{char}=u_{char.rel} \\times C=%f$"
            ), uchar)
          )
        )
        self$results$ruchar$setContent(as.character(uchar))
      }
    )
  )
}
