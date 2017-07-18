#' @title MAIN
#'
#' @description The task of this function is to start the whole analyzation process. The analyzation is done several steps which can be seen in the following listing:
#'
#' \enumerate{
#'   \item main
#'     \enumerate{
#'       \item \code{\link{a.a.main}}
#'       \item \code{\link{a.b.main.image.recognition}}
#'       \item \code{\link{a.b.main.image.recognition}}
#'   }
#' }
#' Further more there is the util script for supporting the other scritps:
#' \itemize{
#'   \item \code{\link{z.a.util.gen.summary}}
#'   \item \code{\link{z.b.util.diff}}
#'   \item \code{\link{z.c.boxcox}}
#'   \item \code{\link{z.d.loglik}}
#'   \item \code{\link{z.z.util.set.environment}}
#' }
#'
#' Each of clickable listing elements are functions in the package which have a specific task.
#'
#' A separate conceptional project documentation was created and can be found in the root folder of the project. Therefore this code documentation is kept brief to take that into account.
#'
#' The naming convention with LETTER.LETTER.FUNCTIONNAME has solely the purpose to ensure the correct ordering of the functions in the generated pdf of the documentation which was done by roxygen2.
#' Otherwise the order of function and project descriptions may not follow the linear process of the steps and makes the understanding more difficult.
#'
#' It executes just one function: \code{\link{a.b.main.image.recognition}}
#'
#' @author Vitali Friesen
#' @export
a.a.main <- function(){
  a.b.main.image.recognition()
}

#' @title MAIN - Image Recognition
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' Meta function for starting meta steps of the analysis.
#'
#' Each step will be further explained in the corresponding function descriptions
#'
#' @author Vitali Friesen
a.b.main.image.recognition <- function(){
  library(ggplot2)

  message("0. Loading environment")
  z.z.util.set.environment()
  cat("\n")

  message("1. Preprocessing")
  b.a.preprocessing.start()
  cat("\n")

  message("2. Feature extraction")
  c.a.feature.start()
  cat("\n")

  message("3. Random Forest")
  d.a.randomForest.start()
  cat("\n")

  message("4. Artificial Neural Network")
  e.a.ann.start()
  cat("\n")
  
  message("5. Convolutional Neural Network")
  f.a.cnn.start()
  cat("\n")

  message("6. Evaluation / Comparison")
  g.a.evaluate.start()
  cat("\n")
}
