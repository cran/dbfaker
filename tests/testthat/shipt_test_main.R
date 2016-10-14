library(dbfaker)
context("Type checking validation")

test_that("Testing write verification function", {
  env <- Sys.getenv(c('DATA_DB_SCHEMA','DATA_DB_HOST','DATA_DB_PORT','DATA_DB_USER','DATA_DB_PWD'))
  conn <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         dbname = env['DATA_DB_SCHEMA'],
                         host = env['DATA_DB_HOST'],
                         port = env['DATA_DB_PORT'],
                         user = env['DATA_DB_USER'],
                         password = env['DATA_DB_PWD'])
  if(!RPostgreSQL::isPostgresqlIdCurrent(conn)) {
    test_that::skip("Cannot connect to database for testing.")
  } else {
    info <- getTableInfo(conn, 'public', 'special_request_metro_store')
    data1 <- data.frame(m_id=c(1), s_id=c(-2948824), dt=c('garbage'), avg=c(23.0394839849224), stringsAsFactors=FALSE)
    data2 <- data.frame(m_id=c(1), s_id=c(2), dt=c('2016-08-24'), avg=c(23.03), stringsAsFactors=FALSE)
    expect_error(verifyWrite(conn, "public", "special_request_metro_store", data1, 1))
    expect_silent(verifyWrite(conn, "public", "special_request_metro_store", data2, 1))
    DBI::dbDisconnect(conn)
  }
})
