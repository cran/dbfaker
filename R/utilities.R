# ============= #
# = Utilities = #
# ============= #

# Utility function to generate list of constraint checking functions
createConstraintCheckers <- function(table_info) {
  # generate constraint lists
  num_constraints <- genNumConstraintDf(table_info)
  str_constraints <- genStringConstraintDf(table_info)
  null_constraints <- genNullConstraintDf(table_info)
  date_constraints <- genDateConstraintDf(table_info)

  # generate constraints
  prec_scale_checks <- genPrecisionScaleConstraints(num_constraints)
  generic_range_checks <- genGenericRangeConstraints(num_constraints)
  string_checks <- genStringConstraints(str_constraints)
  null_checks <- genNullConstraints(null_constraints)
  date_checks <- genDateConstraints(date_constraints)

  # return flattened list of constraint checking functions
  nested_constraints <- c(null_checks, prec_scale_checks, generic_range_checks, string_checks, date_checks)
  constraints <- do.call(c, unlist(nested_constraints, recursive=FALSE))
  constraints
}

# Hint to determine resolve parallel backend for cross-platform compatibility
hintParallelBackend <- function(cores) {
  parallel <- FALSE
  os <- Sys.info()['sysname']
  if(os %in% c('Darwin', 'Linux') || .Platform$OS.type == "unix") {
    doMC::registerDoMC(cores=cores)
    parallel <- TRUE
  }
  parallel
}

# ========================== #
# = Assertion Constructors = #
# ========================== #

checkRangeBuilder <- function(column_name, ordinal_position, llimit, ulimit, transform, msg) {
  function(df) {
    # data
    column <- df[[ordinal_position]]

    # identity function as default transform
    if(is.null(transform)) {
      transform <- function(x) {x}
    }

    # catch assertion failure to return informative errors
    result = tryCatch({
      toss <- assertive::assert_all_are_in_range(as.numeric(transform(column)), as.numeric(llimit), as.numeric(ulimit))
    },
    error = function(e) {
      stop(paste0("Potential overflow detected in column ", column_name, ".\n", e))
    })
  }
}

checkMissingBuilder <- function(column_name, ordinal_position, transform, msg) {
  function(df) {
    column <- df[[ordinal_position]]
    if(is.null(transform)) {
      transform <- function(x) {x}
    }
    result = tryCatch({
      toss <- assertive::assert_all_are_not_na(transform(column))
    },
    error = function(e) {
      stop(paste0("One or more missing values in non-nullable column ", column_name, ".\n", e))
    })
  }
}

checkDateBuilder <- function(column_name, ordinal_position, transform, msg) {
  function(df) {
    column <- df[[ordinal_position]]
    if(is.null(transform)) {
      transform <- parsedate::parse_date
    }
    result = tryCatch({
      toss <- assertive::assert_all_are_not_na(transform(column))
    },
    error = function(e) {
      stop(paste0("One or more dates could not be parsed in column ", column_name, ".\n", e))
    })
  }
}
