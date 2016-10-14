# ================ #
# == Table Info == #
# ================ #

getTableList <- function(conn) {
  query <- "SELECT *
  FROM information_schema.tables
  WHERE table_type = 'BASE TABLE'
  AND table_schema NOT IN ('pg_catalog', 'information_schema')"
  schema <- RPostgreSQL::dbGetQuery(conn, query)
  return(schema)
}

getTableInfo <- function(conn, schema, table_name) {
  query <- "SELECT * FROM INFORMATION_SCHEMA.COLUMNS where table_name = '%s' and table_schema = '%s'"
  fmt_query <- do.call(sprintf, as.list(c(query, table_name, schema)))
  table_info <- RPostgreSQL::dbGetQuery(conn, fmt_query)
  return(table_info)
}
