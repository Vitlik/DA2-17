% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ann.R
\name{e.b.ann.step1}
\alias{e.b.ann.step1}
\title{Artificial Neural Network - Model generation}
\usage{
e.b.ann.step1(train_x, train_y, rounds, lr, nodes, batch)
}
\arguments{
\item{train_x}{A data matrix with features to train on, rows being objects and columns being features}

\item{train_y}{A vector containing all classifications for the given objects}

\item{rounds}{A numeric value that is the amount of rounds the model is trained}

\item{lr}{The learning rate}

\item{nodes}{A vector containing node amounts per layer, e.g. rep(1000, 2) for two layers with 1000 nodes.}

\item{batch}{The amount of objects which the model is trained on every round}
}
\value{
ANN model of the package mxnet
}
\description{
Creates the ANN Model from the library "mxnet".
To get (back) to the overview of all steps and functions use this link:
\code{\link{a.a.main}}
}
\author{
Nils Meckmann, Maren Reuter, Sascha di Bernardo
}
