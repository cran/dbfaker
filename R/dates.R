# ================================= #
# = Date / Time / Interval Checks = #
# ================================= #

#' @importFrom dplyr %>%
genDateConstraintDf <- function(table_info) {
  datetimes = c('date', 'timestamp')
  date_constraint_df <- table_info %>%
    dplyr::select(column_name, ordinal_position, data_type) %>%
    dplyr::filter(data_type %in% datetimes) %>%
    dplyr::mutate(ordinal_position = ordinal_position-1)
  date_constraint_df
}

genDateConstraints <- function(date_constraint_df) {
  funcs <- c()
  if(as.logical(nrow(date_constraint_df))) {
    funcs <- apply(date_constraint_df[,c('column_name', 'ordinal_position', 'data_type')], 1, function(y) {
      msg <- "If you're seeing this, your date is prob fucked up, since this is sloppy/lax date parsing.\n
      Does your data look like it conforms to the type %s?"
      fmt_msg = do.call(sprintf, as.list(c(msg, y['data_type'])))
      checkDateBuilder(y['column_name'], as.numeric(y['ordinal_position']), NULL, fmt_msg)
    })
  }
  funcs
}

