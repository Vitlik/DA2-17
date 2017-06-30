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
  library(devtools)
  # Explanation
  c.b.colorHist(16)

  # Explanation
  c.c.step2()

  # Histogram of Oriented Gradients
  c.d.hog()
}

#' @title Feature Extraction - Step 1
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Vitali Friesen
c.b.colorHist <- function(buckets){
  library(png)
  # load image list
  imgList <- list.files("data-raw/IMG/CS CZ",full.names = T, ignore.case = F, recursive = T)
  #imgList <- list.files("data-raw/IMG/CS CZ halved",full.names = T, ignore.case = F, recursive = T)
  #imgList <- list.files("data-raw/IMG/CS CZ quarter",full.names = T, ignore.case = F, recursive = T)
  #imgList <- list.files("data-raw/IMG/CS CZ eighth",full.names = T, ignore.case = F, recursive = T)
  # set number of buckets per color
  # calculate for each image
  colHist <- t(sapply(imgList, function(imgPath){
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
  colnames(colHist) <- c(paste0("r",1:buckets), paste0("g",1:buckets), paste0("b",1:buckets))
  
  # sort rows by their row names
  colHist <- colHist[ order(row.names(colHist)), ]

  # store  variable in file
  colorHistOriginal <- colHist
  devtools::use_data(colorHistOriginal, overwrite = T)
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


#' @title Feature Extraction - Step 2 (Histogram of Oriented Gradients)
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Sascha Di Bernardo
c.d.hog <- function(){
  
  library(OpenImageR)
  
  # applies HOG to all images of the workspace
  hog1 = HOG_apply("C:/Users/Sascha/Documents/DA2-17-Images/", cells = 16, orientations = 4)
  save(hog,file="hog_16.Rda")
  
  hog2 = HOG_apply("C:/Users/Sascha/Documents/DA2-17-Images/", cells = 9, orientations = 4)
  save(hog,file="hog_9.Rda")
  

}
