# This file is a generated template, your changes will not be overwritten

NormalityTestKSClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "NormalityTestKSClass",
    inherit = NormalityTestKSBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        x <- self$data[, self$options$x]
        if(self$options$adjust){
          ks<-lillie.test(x)
        }else{
          ks<-ks.test(x, "pnorm", mean(x), sd(x))
        }
        self$results$result$setContent(
          c(
            sprintf("statistic = %f", ks$statistic),
            sprintf("p         = %f", ks$p.value)
          )
        )
      }
    )
  )
}
