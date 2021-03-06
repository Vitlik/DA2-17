% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature.R
\name{c.e.displayHogFeature}
\alias{c.e.displayHogFeature}
\title{Feature Extraction - display HOG (Histogram of Oriented Gradients)}
\usage{
\method{c}{e.displayHogFeature}(imgPath, imgName, hogPath, cells, orientations)
}
\arguments{
\item{imgPath}{A string that holds a path to the image that should be visualized}

\item{imgName}{A string that holds the name of the image (the last part of the path)}

\item{hogPath}{A string that holds a path to the hog feature data}

\item{cells}{A number that describes the hog folding and by that into how many \code{cells*cells} the image will be rastered}

\item{orientations}{A number that describes how many orientation vectors are stored for each hog cell. This number is for the vectors from 1?-180?}
}
\description{
To get (back) to the overview of all steps and functions use this link:
\code{\link{a.a.main}}
}
\examples{
\\code{c.e.displayHogFeature("C:/Users/t_tobo01/Git/DA2-17/data-raw/IMG/CS CZ/normal/Vit/hl 2017-06-14 18-06-49-48.png", 
"hl 2017-06-14 18-06-49-48.png", "data/hog_original_12_9_complete.Rda", 12, 9)}
}
\author{
Tassilo Tobollik
}
