
#' @title Util - Generate Summary
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#' Generate a summary of given data or compare two
#'
#' This function generates a summary of a data.frame with one or two given tables with data.
#' Each column/attribute of the given data will be summarized individually.
#'
#' It basically works like the standard summary function of the \code{base} package but it does not store the row description in each individual cell like it is done in the standard function.
#'
#' Furthermore it also compares the summary of to data tables if two are passed as parameters.
#'
#' @param frame1 A data.frame or matrix which get summarzed.
#' @param f1_type A name of type character for the context the observations in the data represent. These could by "Spam" for example. It then gets prefixed to the element of summarization of the row like "Spam Mean:".
#'
#' I will only be used if one data table is provided. For two data table it will be "Diff" by standard and will create row names like "Diff Mean:".
#' @param frame2 A data.frame or matrix which get summarzed and compared to frame1. Number and name of the columns of \code{frame1} and \code{frame2} need to be identical
#'
#' @return A data.frame with either 6 rows for ein provided data.frame with the elements min, 1st quantile, median, mean, 3rd quantile and max.
#'
#' For two data.frames it contains 20 rows with 6 rows looking explained above, another 6 rows showing the difference of each summary element of the two data tables and 2 rows of empty space between each summary.
#'
#' @author Vitali Friesen
z.a.util.gen.summary <- function(frame1, f1_type, frame2 = NULL, ...){
  stopifnot(is.data.frame(frame1) | is.matrix(frame1))
  # only one suitable data table is given
  if(is.null(frame2) | (!is.data.frame(frame2) | !is.matrix(frame2)) ){
    # create a data.frame
    # one sapply for each row of the summary
    tmp <- rbind(
      # calculate a vector of mins
      sapply(1:ncol(frame1), function(column){min(frame1[,column])}),
      rbind(
        # calculate a vector of 25% quantiles
        sapply(1:ncol(frame1), function(column){
          quantile(frame1[,column],probs = 0.25)}),
        rbind(
          # calculate a vector of medians
          sapply(1:ncol(frame1), function(column){median(frame1[,column])}),
          rbind(
            # calculate a vector of means
            colMeans(frame1),
            rbind(
              # calculate a vector of 75% quantiles
              sapply(1:ncol(frame1), function(column){
                quantile(frame1[,column],probs = 0.75)}),
              # calculate a vector of maxs
              sapply(1:ncol(frame1), function(column){max(frame1[,column])})
            )
          )
        )
      )
    )
    # round values for better readability
    tmp <- round(tmp,4)
    # set column names to the same as the source tables
    colnames(tmp) <- colnames(frame1)
    # set row names corresponding to the type of information they contain
    row.names(tmp) <- c(paste0(f1_type," Min:"), paste0(f1_type," 1st Qu.:"),
                        paste0(f1_type," Median:"), paste0(f1_type," Means:"),
                        paste0(f1_type," 3rd Qu.:"), paste0(f1_type," Max:"))
    return(tmp)
  }
  # two data tables provided
  else {
    # stop if both tables do not have the same attributes
    stopifnot(colnames(frame1) == colnames(frame2))
    # calculate the difference between both summaries
    tmp <- rbind(
      z.b.util.diff(
        sapply(1:ncol(frame1), function(column){min(frame1[,column])}),
        sapply(1:ncol(frame2), function(column){min(frame2[,column])})
      ),
      rbind(
        z.b.util.diff(
          sapply(1:ncol(frame1), function(column){
            quantile(frame1[,column],probs = 0.25)}),
          sapply(1:ncol(frame2), function(column){
            quantile(frame2[,column],probs = 0.25)})
        ),
        rbind(
          z.b.util.diff(
            sapply(1:ncol(frame1), function(column){median(frame1[,column])}),
            sapply(1:ncol(frame2), function(column){median(frame2[,column])})
          ),
          rbind(
            z.b.util.diff(
              colMeans(frame1),
              colMeans(frame2)
            ),
            rbind(
              z.b.util.diff(
                sapply(1:ncol(frame1), function(column){
                  quantile(frame1[,column],probs = 0.75)}),
                sapply(1:ncol(frame2), function(column){
                  quantile(frame2[,column],probs = 0.75)})
              ),
              z.b.util.diff(
                sapply(1:ncol(frame1), function(column){
                  max(frame1[,column])}),
                sapply(1:ncol(frame2), function(column){
                  max(frame2[,column])})
              )
            )
          )
        )
      )
    )
    # see above
    colnames(tmp) <- colnames(frame1)
    row.names(tmp) <- c("Diff Min:", "Diff 1st Qu.:", "Diff Median:", "Diff Means:",
                        "Diff 3rd Qu.:", "Diff Max:")
    # create a data.frame with the summary of each of the two given data tables
    # then add the comparing data.frame at the end
    tmp <- rbind(
      rbind(z.a.util.gen.summary(frame1, "NoSpam"), rep("",length(frame1))),
      rbind(
        rbind(z.a.util.gen.summary(frame2, "Spam"), rep("",length(frame1))),
        tmp
      )
    )
    return(tmp)
  }
}

#' @title Util - DIfference
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#' Support function for \code{\link{z.a.util.gen.summary}}
#'
#' Takes two numeric vectors and calculates the relative size of the numbers at the same positions.
#'
#' @param vec1 A numeric vector.
#' @param vec2 A numeric vector of the same size as \code{vec1}
#'
#' @return A character vector containing the relative sizes. Special cases for \code{0} elements in the vector.
#'
#' @examples
#' z.b.util.diff(c(8,0,10,0),c(4,3,15,0))
#' # [1] "50%" "only in 2nd" "150%" "Both 0"
#'
#' @author Vitali Friesen
z.b.util.diff <- function(vec1, vec2){
  stopifnot(length(vec1) == length(vec2) & is.numeric(c(vec1,vec2)))
  return(
    # for each element in the vector
    sapply(1:length(vec1), function(pos){
      ifelse(vec2[pos] != 0,
             ifelse(vec1[pos] != 0,
                    # both are not 0 -> relative size can be calculated
                    paste0(round((vec2[pos]/vec1[pos])*100, 2),"%"),
                    # element in vec1 is 0
                    "only in 2nd"),
             ifelse(vec1[pos] != 0,
                    # element in vec2 is 0
                    "only in 1st",
                    # elements in both vectors are 0
                    "Both 0"))
    })
  )
}

#' @title Util - Boxcox
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#' A function for transforming data with boxcox
#'
#' This function transforms data with the boxcox transformation to reduce the scewness of a random
#' variable..
#'
#' @param data The matrix or data.frame which gets transformed.
#' @param lambda The intensity of the transformation.
#' @return The transformed data
#'
#' @author Vitali Friesen (Credits: Tutorial of Data Analytics 2016 by PROF. DR. HEIKE TRAUTMANN )
z.c.boxcox <- function(data, lambda){
  if (lambda == 0) {
    return(log(data))
  }
  return((data^lambda - 1) / lambda)
}

#' @title Util - Log-likelihood
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#' Log-likelihood function for boxcox transformation
#'
#' It calculates how close the boxcox transformation is to being normal.
#'
#' Can be used to be optimized to find optimal lambda.
#'
#' @author Vitali Friesen (Credits: Tutorial of Data Analytics 2016 by PROF. DR. HEIKE TRAUTMANN )
z.d.loglik = function(lambda, data) {
  n <- length(data)
  boxcoxed <- z.c.boxcox(data, lambda)
  a <- var(boxcoxed)
  b <- sum(log(data))
  return((-n/2) * a + (lambda - 1) * b)
}

#' @title Util - Function that converts RGBA to RGB
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#' This function transforms the given RGBA values to RGB values.
#' Thereby r,g,b are the background RGB colors, r2,g2,b2 are the foreground picture colors of the RGBA image
#' and a is the alpha value of the RGBA image. 
#' @param r An one dimensional dataframe that holds the red colour values of the background of the image
#' @param g An one dimensional dataframe that holds the green colour values of the background of the image
#' @param b An one dimensional dataframe that holds the blue colour values of the background of the image
#' @param a An one dimensional dataframe that holds the alpha values of the image
#' @param r2 An one dimensional dataframe that holds the red colour values of the foreground of the image
#' @param g2 An one dimensional dataframe that holds the green colour values of the foreground of the image
#' @param b2 An one dimensional dataframe that holds the blue colour values of the foreground of the image
#' @return A three dimensional dataframe that holds the converted RGB colour values of the image
#' @author Tassilo Tobollik
z.e.RGBAtoRGB <- function(r = 0, g = 0, b = 0, a, r2,g2,b2){
  r3 <- floor(((1 - a) * r2) + (a * r))
  g3 <- floor(((1 - a) * g2) + (a * g))
  b3 <- floor(((1 - a) * b2) + (a * b))
  rgb <- c(r3,g3,b3)
  names(rgb) <- c("red","green","blue")
  return(rgb)
}

#' @title Util - function that rasters and displays an image
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#' @param image A dataframe that holds the pixel values of the image that should be rendered
#' @param x1 An optional number that describes the first x value of the part of the image that should be rendered
#' @param x2 An optional number that describes the second x value of the part of the image that should be rendered
#' @param y1 An optional number that describes the first y value of the part of the image that should be rendered
#' @param y2 An optional number that describes the second y value of the part of the image that should be rendered
#' @param interpol A logical vector (or scalar) indicating whether to apply linear interpolation to the image when drawing
#' @param noBorder A boolean that describes if the image should be rendered without plot-borders
#' @author Vitali Friesen, Tassilo Tobollik
z.f.displayRgbImage <- function(image, x1 = NULL, x2 = NULL, y1 = NULL, y2 = NULL, interpol = F, noBorder = T){
  if(noBorder){
    #Set graphic parameters so that no borders are displayed
    par(bty = "n", mai=c(0,0,0,0), mar=c(0,0,0,0), usr=c(1,2,1,2), xaxs="i", yaxs="i")
  }
  if(is.null(x1) && is.null(x2) && is.null(y1) && is.null(y2)){
    #Raster the whole image
    plot(1:2, type='n')
    rasterImage(image, 1,1,2,2, interpolate = interpol)#mai or mar 
  }else{
    #Raster just a part of the image
    rasterImage(image[x1:x2,y1:y2,], 1,1,2,2, interpolate = interpol) 
  }
}

#' @title Util - function that reads in an image from the given path
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#' @param path A string that holds a path to an image that should be read
#' @param asPng A boolean that describes if the image is an .png file or another image data type file
#' @return A dataframe that holds the pixel values of the image
#' @author Tassilo Tobollik
z.g.readImage <- (path, asPng = F){
  library(png)
  if(asPng){
    image <- readPNG(path)
  }else{
    #Allows better display uses
    image <- readImage(path)
  }
  return(image)
}

#' @title Util - Environment Setting
#' @description To get (back) to the overview of all steps and functions use this link: \code{\link{a.a.main}}
#'
#' This function simply sets some environment variables so they can be used across the package.
#'
#' @author Vitali Friesen
z.z.util.set.environment <- function(){
  # create outputfolder if not existing yet
  if(!file.exists("out")) dir.create("out")

  # set some colors
  ercis.red      <<- rgb(133/255,  35/255,  57/255, 1)
  ercis.lightred <<- rgb(200/255, 156/255, 166/255, 1)
}
