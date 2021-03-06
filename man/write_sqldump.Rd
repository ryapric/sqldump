% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_sqldump.R
\name{write_sqldump}
\alias{write_sqldump}
\title{Write SQL dump file from data frame}
\usage{
write_sqldump(table_list, outfile)
}
\arguments{
\item{table_list}{Named list of \code{data.frame}s to convert to SQL text. Names will
be the names of the tables created from the dump file.}

\item{outfile}{Dump file destination}
}
\description{
Provided a \code{data.frame}-like object, writes the equivalent SQL dump file to
\code{outfile}.
}
\examples{
 write_sqldump(list(iris = iris), file.path(tempdir(), "dump.sql"))

}
