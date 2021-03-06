% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{verifyWrite}
\alias{verifyWrite}
\title{Verify the validity of writing a dataframe to a Postgres database}
\usage{
verifyWrite(conn, schema, table_name, data, cores = 4)
}
\arguments{
\item{conn}{Postgres database connection object}

\item{schema}{A string specifying the database schema}

\item{table_name}{A string specifying the database table}

\item{data}{A dataframe you are trying to write to a database}

\item{cores}{Controls parallelism of tests; default is fine, don't touch this}
}
\description{
Verify the validity of writing a dataframe to a Postgres database
}

