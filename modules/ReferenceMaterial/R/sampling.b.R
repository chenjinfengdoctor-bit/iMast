# This file is a generated template, your changes will not be overwritten
library(goftest)
SamplingClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "SamplingClass",
    inherit = SamplingBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        cs <- self$data[, self$options$cs]
        cs.raw <- cs
        csid <- self$data[, self$options$csid]
        n <- self$options$n
        l <- self$options$l
        r <- self$options$r
        ratio <- 0.2
        el <- (1 - ratio) * l
        er <- (1 + ratio) * r
        expt <- ad.test(cs, null = "pexp", rate = 1 / mean(cs))
        self$results$text$setContent(expt)
        if (expt$p.value > self$options$alpha) {
          cs <- log10(cs)
          el <- log10(el)
          er <- log10(er)
        }

        target_points <- seq(el, er, length.out = n)
        # ratio <- self$options$ratio / 100

        # el <- l - abs(r - l) * ratio
        # er <- r + abs(r - l) * ratio

        if (min(cs) > el || max(cs) < er) {
          stop(sprintf(
            "CS Cannot Cover [L,R], [%.3f,%.3f] vs [%.3f,%.3f]",
            min(cs), max(cs), el, er
          ))
        }
        sampled_id <- numeric(n)
        idx <- seq_along(cs)
        for (i in seq_len(n)) {
          target <- target_points[i]
          distances <- abs(cs[idx] - target)
          if (length(distances) == 0) {
            stop(
              "No remaining samples to select. Check if n exceeds available data."
            )
          }
          id_in_idx <- which.min(distances)
          actual_id <- idx[id_in_idx]

          sampled_id[i] <- actual_id
          idx <- idx[-id_in_idx]
        }
        lower <- 1 / 3 * (er - el) + el
        higher <- 2 / 3 * (er - el) + el
        llist <- list()
        lidx <- 0
        mlist <- list()
        midx <- 0
        hlist <- list()
        hidx <- 0
        for (sid in sampled_id) {
          if (cs[sid] < lower) {
            llist[[as.character(lidx)]] <- sid
            lidx <- lidx + 1
          } else if (cs[sid] > higher) {
            hlist[[as.character(hidx)]] <- sid
            hidx <- hidx + 1
          } else {
            mlist[[as.character(midx)]] <- sid
            midx <- midx + 1
          }
        }
        ccsid <- as.character(csid)
        for (i in seq_len(max(lidx, midx, hidx))) {
          ic <- as.character(i - 1)
          self$results$sr$addRow(
            rowKey = i,
            values = list(
              no = i,
              lid = ifelse(i > lidx, "", ccsid[llist[[ic]]]),
              lv = ifelse(i > lidx, "", cs.raw[llist[[ic]]]),
              mid = ifelse(i > midx, "", ccsid[mlist[[ic]]]),
              mv = ifelse(i > midx, "", cs.raw[mlist[[ic]]]),
              hid = ifelse(i > hidx, "", ccsid[hlist[[ic]]]),
              hv = ifelse(i > hidx, "", cs.raw[hlist[[ic]]])
            )
          )
        }
        # self$results$text$setContent(c(llist,hlist,mlist))

        unift <- ks.test(cs[sampled_id], "punif", min = el, max = er)
        self$results$ut$setContent(unift)
      }
    )
  )
}
