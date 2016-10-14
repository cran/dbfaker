# =============== #
# = Null Checks = #
# =============== #

#' @importFrom dplyr %>%
genNullConstraintDf <- function(table_info) {
  null_constraint_df <- table_info %>%
    dplyr::select(column_name, ordinal_position, is_nullable) %>%
    dplyr::filter(is_nullable == 'NO') %>%
    dplyr::filter(column_name != 'id') %>%
    dplyr::mutate(ordinal_position = ordinal_position-1)
  null_constraint_df
}

genNullConstraints <- function(null_constraint_df) {
  # register missing value checkers for each column, if there are columns
  funcs <- c()
  if(as.logical(nrow(null_constraint_df))) {
    funcs <- apply(null_constraint_df[,c('column_name', 'ordinal_position', 'is_nullable')], 1, function(y) {
      checkMissingBuilder(y['column_name'], as.numeric(y['ordinal_position']), NULL, "")
    })
  }
  funcs
}
