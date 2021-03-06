% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_sqldump.R
\name{read_sqldump}
\alias{read_sqldump}
\title{Read SQL Dump File}
\usage{
read_sqldump(file)
}
\arguments{
\item{file}{File path to a SQL dump. Should have extension \code{.sql}.}
}
\description{
Read SQL Dump File
}
\examples{
 read_sqldump(system.file("example_data/example-sqlite-dump.sql",
              package = "sqldump", mustWork = TRUE))
 # read_sqldump(system.file("example_data/example-mysql-dump.sql",
 #              package = "sqldump", mustWork = TRUE))
 read_sqldump(system.file("example_data/example-postgresql-dump.sql",
              package = "sqldump", mustWork = TRUE))

}
