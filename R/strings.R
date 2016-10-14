# ================= #
# = String Checks = #
# ================= #

genStringConstraintDf <- function(table_info) {
  string_types <- c('character varying', 'varchar', 'character', 'char')
  string_constraint_df <- table_info %>%
    dplyr::select(column_name, ordinal_position, data_type, character_maximum_length) %>%
    dplyr::filter(data_type %in% string_types) %>%
    dplyr::mutate(ordinal_position = ordinal_position-1)
  string_constraint_df
}

genStringConstraints <- function(string_constraint_df) {
  funcs <- c()
  if(as.logical(nrow(string_constraint_df))) {
    funcs <- apply(string_constraint_df[,c('column_name', 'ordinal_position', 'data_type', 'character_maximum_length')], 1, function(y) {
      fmt_msg <- do.call(sprintf, as.list(c("Character length exceeded (expecting type %s with scale %s)", y['data_type'], y['character_maximum_length'])))
      checkRangeBuilder(y['column_name'], as.numeric(y['ordinal_position']), 0, as.numeric(y['character_maximum_length']), (function(x) nchar(x)), fmt_msg)
    })
  }
  funcs
}


