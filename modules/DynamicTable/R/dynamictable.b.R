# This file is a generated template, your changes will not be overwritten

DynamicTableClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "DynamicTableClass",
    inherit = DynamicTableBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        tc <- self$options$tc
        table_array <- self$results$tables
        for (i in 1:tc) {
          table_name <- paste0("table", i)
          table_array$addItem(table_name)
          table <- table_array$get(table_name)
          table$addColumn(
            name = "v1", title = "v1", type = "integer"
          )
          table$addColumn(
            name = "v2", title = "v2", type = "integer"
          )
          table$addRow(
            rowKey = 1,
            values = list(
              v1 = i,
              v2 = i
            )
          )
        }
      }
    )
  )
}
