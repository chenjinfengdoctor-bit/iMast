# This file is a generated template, your changes will not be overwritten

CSUClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "CSUClass",
    inherit = CSUBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        texformat <- function(str) {
          gsub("([{}])", "\\1", str)
        }
        unames <- unlist(list(
          # uchar = texformat("$u_{char}$"),
          ubb = texformat("$u_{bb}$"),
          ust1 = texformat("$u_{st1}$"),
          ust2 = texformat("$u_{st2}$"),
          ust3 = texformat("$u_{st3}$"),
          ust4 = texformat("$u_{st4}$"),
          ust5 = texformat("$u_{st5}$"),
          ust6 = texformat("$u_{st6}$")
        ))
        u <- unlist(list(
          # uchar = as.numeric(self$options$uchar),
          ubb = self$options$ubb,
          ust1 = self$options$ust1,
          ust2 = self$options$ust2,
          ust3 = self$options$ust3,
          ust4 = self$options$ust4,
          ust5 = self$options$ust5,
          ust6 = self$options$ust6
        ))
        umax <- max(u)
        u_removed <- u[u < 1 / 3 * umax]
        u_remain <- u[u >= 1 / 3 * umax]
        uc <- sqrt(sum(u_remain^2) + self$options$uchar^2)
        self$results$uc$setContent(
          c(
            sprintf(
              texformat(
                "$u_c=\\sqrt{u_{char}^2+u_{bb}^2+u_{st1}^2+u_{st2}^2+u_{st3}^2+u_{st4}^2++u_{st5}^2++u_{st6}^2}=%f$"
              ),
              uc
            ),
            texformat(
              "removed $u(u \\lt \\frac{u_{max}}{3})$:"
            ),
            unames[names(u_removed)]
          )
        )
        self$results$u$setContent(
          sprintf(
            texformat(
              "$U=u_c \\times k=%f$"
            ),
            uc * self$options$k
          )
        )
      }
    )
  )
}
