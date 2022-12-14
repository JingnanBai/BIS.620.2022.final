% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_missing_pattern}
\alias{plot_missing_pattern}
\title{plot missing pattern}
\usage{
plot_missing_pattern(data, nalist)
}
\arguments{
\item{data}{the dataset for plot}

\item{nalist}{the vector of column names which contains missing data}
}
\value{
the aggr obj
}
\description{
This function could be use for plotting the missing proportion
and possible patterns for specific dataset
}
\examples{
data(diabetic_data)
plot_missing_pattern(diabetic_data[1:100, ],
          nalist = c("medical_specialty", "payer_code", "A1Cresult"))
}
