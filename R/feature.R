
#' @title Feature Extraction -  Wrapper function
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' This is a wrapper function for ...
#'
#' It executes these functions:
#' \enumerate{
#'   \item \code{\link{c.b.colorHist}}
#'   \item \code{\link{c.c.step2}}
#' }
#'
#' @author Vitali Friesen
c.a.feature.start <- function(){
  # Explanation
  c.b.colorHist()

  # Explanation
  c.c.step2()
}

#' @title Feature Extraction - Step 1
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Vitali Friesen
c.b.colorHist <- function(){
  library(png)
  # set number of buckets per color
  buckets = 16
  # calculate for each image
  colHist <- t(sapply(imgListQ, function(imgPath){
    # load image information into curImg
    curImg <- as.vector(readPNG(imgPath))
    as.vector(
      # apply count for each RGB color dimension
      sapply(1:3, function(rgb){
        # extract subset of the corresponding color (R, G or B) from the image vector
        tmp = curImg[(if(rgb == 1) 0 else (length(curImg)/3*(rgb-1)+1)) : (length(curImg)/3*rgb)]
        # apply count for each bucket in the RGB dimension
        sapply(1:buckets, function(bucket){
          length(tmp[(if (bucket == 1) (0 <= tmp) else ((bucket-1)/buckets < tmp))
                     & tmp <= bucket/buckets])
        })
      })
    )
  }))
  # cut the path from the row names
  rownames(colHist) <- substr(rownames(colHist),nchar(rownames(colHist))-28,nchar(rownames(colHist)))

  # sort rows by their row names
  colHist <- colHist[ order(row.names(colHist)), ]
}

#' @title Feature Extraction - Step 2
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author
c.c.step2 <- function(){

}
