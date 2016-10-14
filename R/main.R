# == TO DO ==
# + Switch implementations to rely on DBI package utilities; this way, this relies only on someone implementing the
#   DBI interface for us to mask writes (opens up Presto, Redshift, MySQL, etc)
# + Should implement Postgres datetime interpretation algorithm so we can actually check dates properly?
# + Look into Quickcheck / property-based testing stuff to make testing this more elegant
# + Write more tests


# Supress R CMD check throwing notes for NSE
globalVariables(c('column_name', 'ordinal_position', 'data_type', 'is_nullable', 'numeric_precision',
                  'numeric_scale', 'character_maximum_length', 'range_type', 'ulimit', 'llimit', 'f'))



#' Verify the validity of writing a dataframe to a Postgres database
#'
#' @param conn Postgres database connection object
#' @param schema A string specifying the database schema
#' @param table_name A string specifying the database table
#' @param data A dataframe you are trying to write to a database
#' @param cores Controls parallelism of tests; default is fine, don't touch this
#' @importFrom foreach %dopar%
#' @export
verifyWrite <- function(conn, schema, table_name, data, cores=4) {
  # If we can connect to the database ...
  res <- tryCatch({
    toss <- DBI::dbGetQuery(conn, "SELECT TRUE;")
  },
  error = function(err) {
    stop("Can't connect to the database :( Maybe you should just push to master ...")
  })

  # Check if table we are writing to is visible to us
  table_list <- getTableList(conn)
  if(!(table_name %in% table_list$table_name)) {
    stop("This table isn't visible to you or doesn't exist in the database.")
  }

  # Check if we are writing the correct # of values to the table.
  info <- getTableInfo(conn, schema, table_name)
  if(!(length(info$column_name)-1 == ncol(data))) {
    stop("You aren't writing the correct number of columns to the database ...")
  }

  # Create and apply constraint checkers
  constraints <- createConstraintCheckers(info)

  # Apply serially if not parallel
  if(hintParallelBackend()) {
    responses <- foreach::foreach(f=constraints, .errorhandling='pass') %dopar% f(data)
  } else {
    responses <- foreach::foreach(f=constraints, .errorhandling='pass') %do% f(data)
  }

  has_errors <- FALSE
  for(response in responses) {
    if(class(response)[[1]] == "simpleError") {
      warning(response$message)
      has_errors <- TRUE
    }
  }
  if(has_errors) {
    stop("You have potential write errors in your script. Please fix them :)")
  }

}




