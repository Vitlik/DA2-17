% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cnn.R
\name{f.b.cnn.step1}
\alias{f.b.cnn.step1}
\title{Convolutional Neural Network - Step 1}
\usage{
f.b.cnn.step1(CNN_model, train_array, train_y, rounds, lr, batch)
}
\arguments{
\item{CNN_model}{The initial model that gets trained in this function}

\item{train_array}{An data array with features to train on, rows being objects and columns being features}

\item{train_y}{A vector containing all classifications for the given objects}

\item{rounds}{A numeric value that is the amount of rounds the model is trained}

\item{lr}{The learning rate}

\item{batch}{The amount of objects which the model is trained on every round}
}
\value{
CNN model the trained model gets returned
}
\description{
To get (back) to the overview of all steps and functions use this link:
\code{\link{a.a.main}}
}
\author{
Maren Reuter, Nils Meckmann
}
