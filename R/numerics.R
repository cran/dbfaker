# ================== #
# = Numeric Checks = #
# ================== #

precision <- function(x) {
  stopifnot(class(x)=="numeric")
  x <- sub("\\..*$","",x)
  ifelse(as.numeric(x) == 0 | as.character(x) == "", 0, nchar(x))
}

scale <- function(x) {
  stopifnot(class(x)=="numeric")
  x <- sub("^[1-9]\\d*$","",x) # excise whole numbers
  x <- sub("^.*\\.","",x) # excise non-fractional portion
  ifelse(as.numeric(x) == 0 | as.character(x) == "", 0, nchar(x))
}

#' @importFrom dplyr %>%
genNumConstraintDf <- function(table_info) {
  nums <- c('smallint', 'integer', 'bigint', 'decimal', 'numeric', 'real', 'double precision', 'smallserial', 'serial', 'bigserial')
  numeric_constraint_df <- table_info %>%
    dplyr::select(column_name, ordinal_position, data_type, numeric_precision, numeric_scale) %>%
    dplyr::rename(precision=numeric_precision, scale=numeric_scale) %>%
    dplyr::filter(data_type %in% nums) %>%
    dplyr::mutate(ordinal_position = ordinal_position-1) %>%
    dplyr::filter(column_name != 'id') %>%
    tidyr::gather(range_type, ulimit, precision:scale) %>%
    dplyr::select(column_name, ordinal_position, data_type, range_type, ulimit)
  numeric_constraint_df
}

genPrecisionScaleConstraints <- function(numeric_constraint_df) {
  # if there are any numeric columns to be checked, generate checking functions for each column; otherwise, funcs is empty
  funcs <- c()
  if(as.logical(nrow(numeric_constraint_df))) {
    funcs <- apply(numeric_constraint_df[,c('column_name', 'ordinal_position', 'data_type', 'range_type', 'ulimit')], 1, function(y) {
      if(y['range_type'] == 'precision') {
        transform <- precision
      } else {
        transform <- scale
      }
      fmt_msg <- do.call(sprintf, as.list(c("%s exceeded (expecting type %s with %s %s)", y['range_type'], y['data_type'], y['range_type'], y['ulimit'])))
      checkRangeBuilder(y['column_name'], as.numeric(y['ordinal_position']), 0, as.numeric(y['ulimit']), transform, fmt_msg)
    })
  }
  funcs
}

#' @importFrom dplyr %>%
genGenericRangeConstraints <- function(numeric_constraint_df) {
  # mapping range parameters to types
  typeInfo <- list(smallint = list(llimit = -32768, ulimit = 32767, transform = NULL),
                   integer = list(llimit = -2147483648, ulimit = 2147483647, transform = NULL),
                   bigint = list(llimit = -9223372036854775808, ulimit = 9223372036854775807, transform = NULL),
                   smallserial = list(llimit = 1, ulimit = 32767, transform = NULL),
                   serial = list(llimit = 1, ulimit = 2147483647, transform = NULL),
                   bigserial = list(llimit = 1, ulimit = 9223372036854775807, transform = NULL))
  # filter out stuff we would've checked before (aka things with no explicit range, but instead a precision and scale)
  numeric_constraint_df2 <- numeric_constraint_df %>%
    dplyr::filter(data_type %in% c('smallint', 'integer', 'bigint', 'smallserial', 'serial', 'bigserial'))
  # generate constraint checking functions for each column
  funcs <- c()
  if(as.logical(nrow(numeric_constraint_df2))) {
    funcs <- apply(numeric_constraint_df2[,c('column_name', 'ordinal_position', 'data_type')], 1, function(y) {
      typeConstraints <- typeInfo[[y['data_type']]]
      llimit <- typeConstraints$llimit
      ulimit <- typeConstraints$ulimit
      transform <- typeConstraints$transform
      fmt_msg <- do.call(sprintf, as.list(c("Range exceeded (expecting type %s w/ values in [%s, %s])", y['data_type'], llimit, ulimit)))
      params <- c(list(column_name = y['column_name'], ordinal_position = y['ordinal_position'], llimit = llimit, ulimit = ulimit, transform = transform, msg = fmt_msg))
      do.call(checkRangeBuilder, params)
    })
  }
  funcs
}

