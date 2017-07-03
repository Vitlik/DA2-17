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
  folder <- (
    #"data-raw/IMG/CS CZ original/normal/"
    #"data-raw/IMG/CS CZ original/histEqual/"
    #"data-raw/IMG/CS CZ original/rgbNorm/"
    #"data-raw/IMG/CS CZ halved/normal/"
    #"data-raw/IMG/CS CZ quarter/normal/"
    "data-raw/IMG/CS CZ eighth/normal/"
   )
  
  imgList <- list.files(folder, full.names = T, ignore.case = F, recursive = T)
  # calculate for each image
  colorHist <- t(sapply(imgList, function(imgPath){
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
  rownames(colorHist) <- substr(rownames(colorHist),
                         nchar(rownames(colorHist))-28,
                         nchar(rownames(colorHist)))
  # column names
  colnames(colorHist) <- c(paste0("r",1:buckets), 
                           paste0("g",1:buckets), 
                           paste0("b",1:buckets))
  
  # sort rows by their row names
  colorHist <- colorHist[ order(row.names(colorHist)), ]
  
  # store  variable in file
  #colorHistOriginal <- colorHist
  #save(colorHistOriginal, file = "data/colorHistOriginal.rda")
  #colorHistOriginalEqual <- colorHist
  #save(colorHistOriginalEqual, file = "data/colorHistOriginalEqual.rda")
  #colorHistHalved <- colorHist
  #save(colorHistHalved, file = "data/colorHistHalved.rda")
  #colorHistQuarter <- colorHist
  #save(colorHistQuarter, file = "data/colorHistQuarter.rda")
  colorHistEighth <- colorHist
  save(colorHistEighth, file = "data/colorHistEighth.rda")
  
  # create plots for the color Histograms
  # c.c.step2()
}

#' @title Feature Extraction - Step 2
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author
c.c.step2 <- function(){
  
  load("data/blockNum.rda")
  red <- paste0("r", 1:blockNum)
  green <- paste0("g", 1:blockNum)
  blue <- paste0("b", 1:blockNum)
  
  # load("data/colorHistOriginal.rda")
  # data <- colorHistOriginal[1,]
  
  load("data/colorHistOriginalEqual.rda")
  data <- colorHistOriginal[1,]
  
  qplot(data[,1:blockNum], red, stat="bin", geom= "bar")
  qplot(data, y, geom="bar", stat="identity", fill=as.factor(x))
  
  ggplot(data, aes(reorder(col, MeanDecreaseAccuracy), MeanDecreaseAccuracy)) +
    geom_bar(stat = "identity", show.legend = F,
             fill = )
  + coord_flip() + xlab("Importance")
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
  save(hog, file="data/hog_16.Rda")
  
  hog2 = HOG_apply("C:/Users/Sascha/Documents/DA2-17-Images/", cells = 9, orientations = 4)
  save(hog, file="data/hog_9.Rda")
}
