% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_predictions.R
\name{load_data}
\alias{load_data}
\title{Load Data}
\usage{
load_data(data_fp)
}
\arguments{
\item{data_fp}{A character string giving the file path of the csv file}
}
\value{
A dataframe containing the raw data (columns for ID, genus, species, text, and labels)
}
\description{
This function loads a csv file containing the raw data for input into the neural network
}
