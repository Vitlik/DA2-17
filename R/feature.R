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
  c.b.colorHist()

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
c.b.colorHist <- function(){
  library(png)
  
  buckets <- 16
  save(buckets, file = "data/buckets.rda")
  
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
  
  load("data/buckets.rda")
  bucks <- 1:buckets
  
  load("data/colorHistOriginal.rda")
  data <- colorHistOriginal[1, 1:buckets]
  dfRed <- data.frame(red = bucks, data)
  data <- colorHistOriginal[1, (buckets+1):(buckets*2)]
  dfGreen <- data.frame(green = bucks, data)
  data <- colorHistOriginal[1, (buckets*2+1):(buckets*3)]
  dfBlue <- data.frame(blue = bucks, data)
  
  ggplot(dfRed, aes(red, data)) + geom_bar(stat = "identity", fill = "#FF0000") + 
    scale_y_continuous(limits = c(0,60000)) 
  ggsave("plots/colorHistOriginalRed.png")
  ggplot(dfGreen, aes(green, data)) + geom_bar(stat = "identity", fill = "#00FF00") + 
    scale_y_continuous(limits = c(0,60000)) 
  ggsave("plots/colorHistOriginalGreen.png")
  ggplot(dfBlue, aes(blue, data)) + geom_bar(stat = "identity", fill = "#0000FF") + 
    scale_y_continuous(limits = c(0,60000)) 
  ggsave("plots/colorHistOriginalBlue.png")
  
  load("data/colorHistOriginalEqual.rda")
  data <- colorHistOriginalEqual[1, 1:buckets]
  dfRed <- data.frame(red = bucks, data)
  data <- colorHistOriginalEqual[1, (buckets+1):(buckets*2)]
  dfGreen <- data.frame(green = bucks, data)
  data <- colorHistOriginalEqual[1, (buckets*2+1):(buckets*3)]
  dfBlue <- data.frame(blue = bucks, data)
  
  ggplot(dfRed, aes(red, data)) + geom_bar(stat = "identity", fill = "#FF0000") + 
    scale_y_continuous(limits = c(0,60000)) 
  ggsave("plots/colorHistOriginalEqualRed.png")
  ggplot(dfGreen, aes(green, data)) + geom_bar(stat = "identity", fill = "#00FF00") + 
    scale_y_continuous(limits = c(0,60000)) 
  ggsave("plots/colorHistOriginalEqualGreen.png")
  ggplot(dfBlue, aes(blue, data)) + geom_bar(stat = "identity", fill = "#0000FF") + 
    scale_y_continuous(limits = c(0,60000)) 
  ggsave("plots/colorHistOriginalEqualBlue.png")
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
  library(parallel)
  
  # applies HOG to all images of the workspace
  hog1 = HOG_apply("C:/Users/Sascha/Documents/DA2-17-Images/", cells = 16, orientations = 4)
  save(hog, file="data/hog_16.Rda")
  
  hog2 = HOG_apply("C:/Users/Sascha/Documents/DA2-17-Images/", cells = 9, orientations = 4)
  save(hog, file="data/hog_9.Rda")
  
  numCores <- detectCores()

  hog3 = HOG_apply("C:/Users/t_tob_000/Git/DA2-17/data-raw/IMG/CS CZ/Colin/", cells = 4, orientations = 9)
  save(hog3, file="data/hog_original_4_9_Colin.Rda")
  hog3 = HOG_apply("C:/Users/t_tob_000/Git/DA2-17/data-raw/IMG/CS CZ/Maren/", cells = 4, orientations = 9)
  save(hog3, file="data/hog_original_4_9_Maren.Rda")
  hog3 = HOG_apply("C:/Users/t_tob_000/Git/DA2-17/data-raw/IMG/CS CZ/Nils/", cells = 4, orientations = 9)
  save(hog3, file="data/hog_original_4_9_Nils.Rda")
  hog3 = HOG_apply("C:/Users/t_tob_000/Git/DA2-17/data-raw/IMG/CS CZ/Nils2/", cells = 4, orientations = 9, threads = numCores)
  save(hog3, file="data/hog_original_4_9_Nils2.Rda")
  hog3 = HOG_apply("C:/Users/t_tob_000/Git/DA2-17/data-raw/IMG/CS CZ/Sascha/", cells = 4, orientations = 9)
  save(hog3, file="data/hog_original_4_9_Sascha.Rda")
  hog3 = HOG_apply("C:/Users/t_tob_000/Git/DA2-17/data-raw/IMG/CS CZ/Tac/", cells = 4, orientations = 9)
  save(hog3, file="data/hog_original_4_9_Tac.Rda")
  hog3 = HOG_apply("C:/Users/t_tob_000/Git/DA2-17/data-raw/IMG/CS CZ/Vit/", cells = 4, orientations = 9)
  save(hog3, file="data/hog_original_4_9_Vit.Rda")
  
  load("data/hog_original_4_9_Colin.Rda")
  colin <- hog3
  load("data/hog_original_4_9_Maren.Rda")
  maren <- hog3
  load("data/hog_original_4_9_Nils.Rda")
  nils <- hog3
  load("data/hog_original_4_9_Nils2.Rda")
  nils2 <- hog3
  load("data/hog_original_4_9_Sascha.Rda")
  sascha <- hog3
  load("data/hog_original_4_9_Tac.Rda")
  tac <- hog3
  load("data/hog_original_4_9_Vit.Rda")
  vit <- hog3
  hogData <- colin
  hogData$files <- c(colin$files, maren$files, nils$files, nils2$files, sascha$files, tac$files, vit$files)
  hogData$hog <- rbind(colin$hog, maren$hog, nils$hog, nils2$hog, sascha$hog, tac$hog, vit$hog)
  save(hogData, file="data/hogData_original_4_9_complete.Rda")
}