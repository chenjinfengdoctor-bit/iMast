# This file is a generated template, your changes will not be overwritten

TOSTSampleSizeClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "TOSTSampleSizeClass",
    inherit = TOSTSampleSizeBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        pow <- self$options$power
        alpha <- self$options$alpha
        R <- self$options$r
        delta <- self$options$delta
        EL <- self$options$sel
        s1_CS <- self$options$cssd1
        s2_CS <- self$options$cssd2
        s1_CRM <- self$options$rmsd1
        s2_CRM <- self$options$rmsd2
        NSF_CS <- self$options$csnsf
        NSF_CRM <- self$options$rmnsf
        s1 <- NSF_CRM * sqrt(s1_CRM^2 + s2_CRM^2)
        s2 <- NSF_CS * sqrt(s1_CS^2 + s2_CS^2)
        uEL <- abs(EL)
        lEL <- -uEL
        if (EL == 0) {
          stop("Equivalence limit may not be zero!")
        } else if (delta <= lEL || delta >= uEL) {
          stop("Delta must be within equivalence limits!")
        }
        n1 <- 1
        diff <- -1
        while (diff < 0) {
          n1 <- n1 + 1
          n2 <- ceiling(R * n1)
          s_pooled <- sqrt((s1^2 / n1) + (s2^2 / n2))
          div1 <- s1^4 / (n1^2 * (n1 - 1))
          div2 <- s2^4 / (n2^2 * (n2 - 1))
          delta_L <- (delta - lEL) / s_pooled
          delta_U <- (delta - uEL) / s_pooled
          df <- ceiling((s_pooled^4) / (div1 + div2))
          Q <- qt(p = 1 - alpha, df = df, ncp = 0)
          PL <- pt(q = Q, df = df, ncp = delta_L)
          PU <- pt(q = -Q, df = df, ncp = delta_U)
          gamma <- PU - PL
          diff <- gamma - pow
        }
        o.n1 <- n1
        o.n2 <- n2
        o.gamma <- gamma
        self$results$sse$items$t$addRow(
          rowKey = "1",
          values = list(
            nrm = n1,
            srm = s1,
            ncs = n2,
            scs = s2,
            sp = s_pooled,
            v = df,
            q = qt(p = 1 - alpha, df = df, ncp = 0),
            pl = pt(q = Q, df = df, ncp = delta_L),
            pu = pt(q = -Q, df = df, ncp = delta_U),
            p = gamma
          )
        )
        self$results$sse$items$ocr$setContent(
          c(
            sprintf("Total sample size for reference materials: $n(RM) = %d$", n1),
            sprintf("Total sample size for clinical samples: $n(CS) = %d$", n2)
            # ,sprintf("actual Power \t= %s", ftrim(gamma))
          )
        )
        input <- c(
          "rep" = "repetitions",
          "pos" = "positions"
        )
        rev_input <- c(
          "rep" = "positions",
          "pos" = "repetitions"
        )
        v <- self$options$v
        ceil <- ceiling(n1 / v)
        if (self$options$input == "rep") {
          rep <- v
        } else {
          rep <- ceil
        }

        n1.c <- ceil * v
        n1 <- n1.c
        c2 <- ceiling(n2 / rep)
        n2.c <- c2 * rep
        n2 <- n2.c
        s_pooled <- sqrt((s1^2 / n1) + (s2^2 / n2))
        div1 <- s1^4 / (n1^2 * (n1 - 1))
        div2 <- s2^4 / (n2^2 * (n2 - 1))
        delta_L <- (delta - lEL) / s_pooled
        delta_U <- (delta - uEL) / s_pooled
        df <- ceiling((s_pooled^4) / (div1 + div2))
        Q <- qt(p = 1 - alpha, df = df, ncp = 0)
        PL <- pt(q = Q, df = df, ncp = delta_L)
        PU <- pt(q = -Q, df = df, ncp = delta_U)
        gamma <- PU - PL
        self$results$sse$items$rop$setContent(
          c(
            tprintf("Specified number of reference material's %s: %d", input[[self$options$input]], v),
            paste0(
              tprintf(
                "Calculated number of reference material's %s: %d = $\\lceil n(RM)/%d \\rceil$",
                rev_input[[self$options$input]], ceil, v
              )
              # ,ifelse(
              #   self$options$only1,
              #   tprintf(
              #     paste0(
              #       "\nAs each reference material is only used for one measurement procedure, ",
              #       "the number of reference materials should be two times that of the procedures: \n",
              #       "$n(RM) = %d \\times %d = %d$"
              #     ),
              #     2, n1, 2 * n1
              #   ), ""
              # )
            ),
            tprintf(
              "Calculated number of clinical sample with %d repetitions: $%d = \\lceil n(CS)/%d \\rceil$",
              rep, c2, rep
            ),
            tprintf(
              "Calculated actual Power with $n(RM) = %d \\times %d = %d,n(CS) = %d \\times %d = %d$:",
              ceil, v, n1.c, c2, rep, n2.c
            )
            # ,tprintf("actual Power = %s", ftrim(gamma))
          )
        )
        self$results$sse$items$p$setContent(
          c(
            tprintf(
              paste0(
                "For given $n(RM)$ and its standard deviation $s_{RM}$, $n(CS)$ and its standard deviation $s_{CS}$, ",
                "the pooled standard deviation:\n",
                "$s_{pooled} = \\sqrt{s_{RM}^2/n(RM)+s_{CS}^2/n(CS)}$\n",
                "the intermediate values $i_{RM},i_{CS}$ for calculating degrees of freedom $v$:\n",
                "$i_{RM} = s_{RM}^4(n(RM)-1)/n(RM)^2,i_{CS} = s_{CS}^4(n(CS)-1)/n(CS)^2$\n",
                "The degrees of freedom $v$ is calculated by Welch-Satterthwaite's:\n",
                "$v = \\lceil s_{pooled}^4/(i_{RM}+i_{CS}) \\rceil$\n",
                "The noncentrality parameters of two t-distributions are calculated by $\\Delta = %s,E_L = %s$:\n",
                "$\\Delta L = (\\Delta - E_L)/s_{pooled}^2,\\Delta U = (\\Delta + E_L)/s_{pooled}^2$\n",
                "Then the Q value is: $Q = t_{1-\\alpha,v}$\n",
                "The actual $Power$ can be calculated by two noncentral t-distributions' CDF:\n",
                "$P_L = F_{t,v,\\Delta L}(Q),P_U = F_{t,v,\\Delta U}(-Q),Power = P_U-P_L$\n"
              ),
              ftrim(delta), ftrim(EL)
            )
          )
        )
        self$results$sse$items$t$addRow(
          rowKey = "2",
          values = list(
            nrm = n1,
            srm = s1,
            ncs = n2,
            scs = s2,
            sp = s_pooled,
            v = df,
            q = qt(p = 1 - alpha, df = df, ncp = 0),
            pl = pt(q = Q, df = df, ncp = delta_L),
            pu = pt(q = -Q, df = df, ncp = delta_U),
            p = gamma
          )
        )
        self$results$to$items$d$setContent(
          tprintf("A feasible measurement sequence is designed as follows:")
        )
        self$results$to$items$c$setContent(
          c(
            tprintf("Where $R_i$ means the $i^{th}$ reference material and $C_j$ means the $j^{th}$ clinical sample."),
            tprintf("The number of reference materials: %d", n1.c),
            tprintf("The number of clinical samples: %d", n2.c)
          )
        )
        set.seed(1)
        um <- ums(n1.c, n2.c)
        ot <- self$results$to$items$t
        row_a_col <- 10
        n <- n1.c + n2.c
        cn <- ceiling(n / row_a_col)
        nc <- min(4, cn)
        rc <- ceiling(n / nc)
        for (i in 1:nc) {
          ot$addColumn(
            name = paste0("t", i),
            title = "ID",
            type = "text"
          )
          ot$addColumn(
            name = paste0("s", i),
            title = "Sample ID",
            type = "text"
          )
        }
        for (i in 1:rc) {
          vs <- list()
          for (j in 1:nc) {
            rid <- i + rc * (j - 1)
            if (rid > nrow(um)) {
              vs[[paste0("t", j)]] <- ""
              vs[[paste0("s", j)]] <- ""
            } else {
              vs[[paste0("t", j)]] <- as.integer(rid)
              vs[[paste0("s", j)]] <- tprintf(
                "$%s_{%d}$",
                um[rid, "source"],
                um[rid, "id"]
              )
            }
          }
          ot$addRow(
            rowKey = i,
            values = vs
          )
        }
      }
    )
  )
}
