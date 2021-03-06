% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delete.R
\name{delete-records}
\alias{delete-records}
\alias{delete_records}
\alias{delete_record}
\title{Delete}
\usage{
delete_records(records, ids, base = NULL, table = NULL,
  quiet = !interactive())

delete_record(id, base = NULL, table = NULL, quiet = !interactive())
}
\arguments{
\item{records}{A \code{data.frame} of records to create.}

\item{base}{Base, whatever this is Airtable does not document it.}

\item{table}{Name of table.}

\item{quiet}{Set to \code{TRUE} to print helpful messages.}

\item{id, ids}{Bare column name containing record ids to update.}
}
\description{
Delete a record
}
