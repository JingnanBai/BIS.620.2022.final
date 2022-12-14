% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_mosaic}
\alias{plot_mosaic}
\title{give mosaic plot for given columns}
\usage{
plot_mosaic(data, ycol, xcol, ncol = 3)
}
\arguments{
\item{data}{the dataset for plot}

\item{ycol}{the column name for the target variable}

\item{xcol}{a vector of column names, the function would draw mosaic plot
for each variable in xcol with ycol (the target one)}

\item{ncol}{the number of plots to show per row}
}
\description{
This function could be used for giving a series of mosaic plots
for given columns, to explore the relationship between the given variables
and the target column (denote as ycol)
}
\examples{
data(diabetic_data)
plot_mosaic(diabetic_data[1:100, ], "readmitted", c("insulin"), ncol = 3)
}
