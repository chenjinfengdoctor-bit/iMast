# This file is a generated template, your changes will not be overwritten

StockingClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "StockingClass",
    inherit = StockingBase,
    private = list(
      .run = function() {
        is.mol <- function(munit) {
          endsWith(munit, "mol")
        }
        stock <- function(res) {
          res$stc <- res$tc * res$sr
          res$sm <- res$stc * res$sv
          res$smg <- res$sm
          if (is.mol(res$munit)) {
            res$smg <- res$smg * res$mw
          }
          return(res)
        }
        set.to.table <- function(res, t, level = 1) {
          t$setTitle(tprintf("%d-grade stock buffer", level))
          t$addRow(rowKey = "wsc", values = list(
            p = "Working buffer", u = tprintf("%s/%s", res$munit, res$vunit), v = sp(res$tc)
          ))
          t$addRow(rowKey = "sc", values = list(
            p = "Stock buffer", u = tprintf("%s/%s", res$munit, res$vunit), v = sp(res$stc)
          ))
          t$addRow(rowKey = "sv", values = list(u = res$vunit, p = "Stock volume", v = sp(res$sv)))
          if (is.mol(res$munit)) {
            t$addRow(rowKey = "nmol", values = list(p = "Total molecular mass", u = res$munit, v = sp(res$sm)))
            t$addRow(rowKey = "mw", values = list(
              p = "Molecular weight", u = tprintf("%s/%s", res$munit, res$vunit), v = sp(res$mw)
            ))
          }
          t$addRow(rowKey = "mg", values = list(p = "Material weight", u = res$munit, v = sp(res$smg)))
        }
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        res <- list(
          tc = self$options$tc,
          sr = self$options$sr,
          munit = self$options$munit,
          vunit = self$options$vunit,
          sv = self$options$sv,
          mw = self$options$mw,
          wth = self$options$wth
        )
        a <- self$results$a
        a$setTitle("")
        level <- 1
        while (TRUE) {
          res <- stock(res)
          tn <- paste0("t", level)
          a$addItem(tn)
          set.to.table(res, a$get(tn), level)
          if (res$smg > res$wth) {
            break
          }
          res$tc <- res$smg / res$sv
          level <- level + 1
          res$munit <- gsub("mol", "g", res$munit)
          res$sr <- 100
        }
        return(TRUE)
        result <- c()
        t <- self$results$t
        t$addRow(
          rowKey = "wsc",
          values = list(
            p = "Working buffer",
            u = tprintf("%s/mL", unit),
            v = sp(tc)
          )
        )
        stc <- sr * tc
        t$addRow(
          rowKey = "sc",
          values = list(
            u = tprintf("%s/mL", unit),
            p = "Stock buffer",
            v = sp(stc)
          )
        )
        t$addRow(
          rowKey = "sv",
          values = list(
            u = "mL",
            p = "Stock volume",
            v = sp(sv)
          )
        )
        result <- c(
          result,
          tprintf(
            "For given working buffer %s %s/mL, the stock buffer is %s %s/mL (%s $\\times$ %s).",
            sp(tc), unit, sp(stc), unit, sp(tc), ftrim(sr)
          )
        )
        sm <- stc * sv
        result <- c(
          result,
          tprintf(
            "To prepare %s mL of a stock buffer of %s %s/mL, %s %s of the material are required.",
            sp(sv), sp(stc), unit, sp(sm), unit
          )
        )
        if (unit == "mmol") {
          t$addRow(
            rowKey = "nmol",
            values = list(
              p = "Total molecular mass",
              u = unit,
              v = sp(sm)
            )
          )
          t$addRow(
            rowKey = "mw",
            values = list(
              p = "Molecular weight",
              u = "g/mol",
              v = sp(mw)
            )
          )
          smg <- sm * mw

          result <- c(
            result,
            tprintf(
              "With material's molecular weight of %s g/mol, %s mmol material equals %s mg (%s $\\times$ %s).",
              sp(mw), sp(sm), sp(smg), sp(sm), sp(mw)
            )
          )
        } else {
          smg <- sm
        }
        t$addRow(
          rowKey = "mg",
          values = list(
            p = "Material weight",
            u = "mg",
            v = sp(smg)
          )
        )
        c.smg <- smg
        l <- 1
        while (c.smg < wth) {
          l <- l + 1
          result <- c(
            result,
            tprintf(
              "Since the weight (%s mg) is less than %s mg, %s mg material should first be used to prepare %s mL of %d-grade stock buffer.",
              sp(c.smg), sp(wth), sp(c.smg * 100), sp(sv), l
            )
          )

          c.smg <- c.smg * 100
          t$addRow(
            rowKey = l,
            values = list(
              p = tprintf("Material weight<br>(%d-grade stock buffer)", l),
              u = "mg",
              v = sp(c.smg)
            )
          )
        }
        self$results$text$setContent(result)
      }
    )
  )
}
