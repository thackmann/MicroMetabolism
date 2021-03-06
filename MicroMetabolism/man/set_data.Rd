% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_predictions.R
\name{set_data}
\alias{set_data}
\title{Set Data}
\usage{
set_data(ID, text_seqs_padded, labels, train_fraction = 1)
}
\arguments{
\item{ID}{A numeric vector IDs with one element per species}

\item{text_seqs_padded}{A matrix of tokenized and padded text}

\item{labels}{A numeric vector of data labels with one element per species (1=trait positive, 0=trait negative, NA=trait not labeled)}

\item{train_fraction}{A numeric scalar giving the fraction of data to be split off for training; if set to 1, all data is used for training except those with no labels}
}
\value{
A list of training and evaluation data and their species IDs
}
\description{
This function splits labels and text into training and evaluation data for input into the network.
}
