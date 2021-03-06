% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql_functions.R
\name{insert_run}
\alias{insert_run}
\title{Insert CEMiTool run information}
\usage{
insert_run(pool, study_id, results_dir, run_date)
}
\arguments{
\item{study_id}{Character string with the GEO accession number to be added}

\item{date}{Date and time the CEMiTool analysis was run}

\item{results}{Path to the CEMiTool report file}

\item{plots}{Path to the CEMiTool plots directory}

\item{reports}{Path to the CEMiTool reports directory}
}
\description{
This function takes information about a call to the CEMiTool.R script
and adds the information into the cemitool_run table in the database.
}
