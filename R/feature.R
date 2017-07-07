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
#' Set working directory to the package before using this function (setwd())
#' ...
#'
#' @author Sascha Di Bernardo, Tassilo Tobollik
c.d.hog <- function(cells, orientations){
  
  library(OpenImageR)
  library(parallel)
  
  folder <- "data-raw/IMG/CS CZ/normal"
  
  #The following works only for folders with images in them not with ones with subfolders.
  dirList <- list.dirs(folder,full.names = T, recursive = F)
  
  fileFolderName <- paste(paste(paste(paste("data/hog_original_", cells, sep = "")
                                      , "_", sep = "")
                                , orientations, sep = "")
                          , "_", sep = "")
  fileNames <- sapply(1:length(dirList), function(y){
    path <- dirList[y]
    namePos <- gregexpr(folder,path)
    name <- substr(path, attr(namePos[[1]], "match.length") + 2, nchar(path))
    hog <- HOG_apply(paste(path, "/", sep = ""), cells = cells, orientations = orientations)
    fileName <- paste(paste(fileFolderName
                      , name, sep = "")
                , ".Rda", sep = "")
    save(hog, file=fileName)
    return(fileName)
  })
  
  for (fileName in fileNames){
    currHog <- load(fileName)
    # namePos <- gregexpr(fileFolderName,fileName)
    # name <- substr(fileName, attr(namePos[[1]], "match.length") + 1, nchar(fileName) - 4)
    if(which(fileNames == fileName) == 1){
      hogData <- hog
    } else {
      hogData$files <- c(hogData$files, hog$files)
      hogData$hog <- c(hogData$hog, hog$hog)
    }
  }
  save(hogData, file = paste(fileFolderName, "complete.Rda", sep = ""))
}

image <- readPNG("C:/Users/TTobo_000/Git/DA2-17/data-raw/IMG/CS CZ/normal/Colin/hl 2017-06-14 17-44-58-48.png")
z.f.displayRgbImage(image=image)
load("data/hog_original_8_9_complete.Rda")
hogFeature <- hogData$hog[1:(8*8*9)]

c.e.displayHogFeature(image, hogFeature, 8, 9)

#' @title Feature Extraction - display HOG (Histogram of Oriented Gradients)
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Tassilo Tobollik
c.e.displayHogFeature <- function(image, hogFeature, cells, orientations){
  
  z.f.displayRgbImage(image)
  
  cellSize = 1/cells
  halfCellSize = cellSize/2
  
  #Draw the cell raster
  abline(v=seq(1,2,by=cellSize), col = "green")
  abline(h=seq(1,2,by=cellSize), col = "green")
  
  #Calculate the cell centers
  cellCenters <- sapply(1:cells, function(x){
    sapply(1:cells, function(y){
      rowNum = floor(x / (cells + 1))
      colNum = floor(y / (cells + 1))
      cellXNum = x - rowNum * cells
      cellYNum = y - colNum * cells
      cellX = 1 + cellXNum * cellSize - halfCellSize
      cellY = 1 + cellYNum * cellSize - halfCellSize
      # points(x=cellX,y=cellY,col="red")
      return(c(cellX,cellY))
    })
  })
  
  #Calculate angles for the vectors
  degree <- 180/orientations
  degrees <- seq(0,180-degree,by=degree)
  degrees <- c(degrees, (180+degrees))
  
  library(grid)
  
  #aaa
  
  #Set a layout for the hog cells
  layout = grid.layout(cells,cells,widths= seq(0.25,0.25,length.out=cells),
                    heights=seq(0.25,0.25,length.out=cells),
                    just='center')

  pushViewport(viewport(layout=layout))#,xscale=2*extendrange(c(min(hogFeature),max(hogFeature)))))#vectorValues[,2])))
  
  #Loop through all cells
  sapply(1:cells, function(cellRow){
    sapply(1:cells, function(cellCol){
      cellNum <- (cellRow - 1) * 4 + cellCol
      #Vector lengths for the current cell
      lengths <- hogFeature[((cellNum-1)*orientations + 1):(cellNum*orientations)]
      #Add vector lengths for 181-360° which are the same
      lengths <- c(lengths,lengths)
      lengths <- lengths*(10^floor(cells/4) * 5)
      vectorValues <- cbind(degrees,lengths)
      c.f.plotHogCellVectors(vectorValues,cellCol,cellRow)
    })
  })
}

#' @title Feature Extraction - plot HOG (Histogram of Oriented Gradients) cell vectors
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'  Function that plots vectors with given angles and length into a specified layout cell
#' ...
#'
#' @author Tassilo Tobollik
c.f.plotHogCellVectors <- function(vectorValues,cellCol,cellRow){
  #Add a new viewport for the specified layout cell
  pushViewport(viewport(layout.pos.col=cellCol,layout.pos.row=cellRow))
  #For each vector
  apply(vectorValues,1,function(x){
    #Add a new viewport with the specified angle
    pushViewport(viewport(angle=x[1])) 
    #Draw a vector with the specified length
    grid.segments(x0=0.5,y0=0.5,x1=0.5+x[2]*0.8,y1=0.5, gp=gpar(col="red"))
    #Remove viewport
    popViewport()
  })
  #Remove viewport
  popViewport()
}
