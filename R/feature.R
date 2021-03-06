#' @title Feature Extraction -  Wrapper function
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' This is a wrapper function for ...
#'
#' It executes these functions:
#' \enumerate{
#'   \item \code{\link{c.b.colorHist}}
#'   \item \code{\link{c.c.colorHistPlotting}}
#' }
#'
#' @author Vitali Friesen
c.a.feature.start <- function(){
  # Explanation
  c.b.colorHist()

  # Explanation
  c.c.colorHistPlotting()

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
  
  # buckets <- 16
  buckets <- 255
  
  # load image list
  folder <- (
    # "data-raw/IMG/CS CZ original/normal/"
    # "data-raw/IMG/CS CZ original/histEqual/"
    # "data-raw/IMG/CS CZ original/rgbNorm/"
    # "data-raw/IMG/CS CZ original/rgbNormHistEqual/"
    # "data-raw/IMG/CS CZ halved/normal/"
    # "data-raw/IMG/CS CZ quarter/normal/"
    # "data-raw/IMG/CS CZ eighth/normal/"
    "data-raw/IMG/CS CZ eighth/rgbNorm/"
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
    #print(paste0(substr(imgPath,nchar(imgPath)-28,nchar(imgPath)), " fertig"))
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
  
  # store  variables in file
  # save(colorHist, buckets, file = "data/colorHistOriginal16Buckets.rda")
  # save(colorHist, buckets, file = "data/colorHistOriginal255Buckets.rda")
  # save(colorHist, buckets, file = "data/colorHistOriginalEqual16Buckets.rda")
  # save(colorHist, buckets, file = "data/colorHistOriginalEqual255Buckets.rda")
  # save(colorHist, buckets, file = "data/colorHistOriginalRGBNorm16Buckets.rda")
  # save(colorHist, buckets, file = "data/colorHistOriginalRGBNorm255Buckets.rda")
  # save(colorHist, buckets, file = "data/colorHistOriginalEqualRGBNorm16Buckets.rda")
  # save(colorHist, buckets, file = "data/colorHistOriginalEqualRGBNorm255Buckets.rda")
  # save(colorHist, buckets, file = "data/colorHistHalved16Buckets.rda")
  # save(colorHist, buckets, file = "data/colorHistHalved255Buckets.rda")
  # save(colorHist, buckets, file = "data/colorHistQuarter16Buckets.rda")
  # save(colorHist, buckets, file = "data/colorHistQuarter255Buckets.rda")
  # save(colorHist, buckets, file = "data/colorHistEighth16Buckets.rda")
  # save(colorHist, buckets, file = "data/colorHistEighth255Buckets.rda")
  # save(colorHist, buckets, file = "data/colorHistEighthRGBNorm16Buckets.rda")
  save(colorHist, buckets, file = "data/colorHistEighthRGBNorm255Buckets.rda")
  
  # create plots for the color Histograms
   #c.c.colorHistPlotting()
}

#' @title Feature Extraction - Plotting Histograms
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Vitali Friesen
c.c.colorHistPlotting <- function(picture = "hl 2017-06-14 17-44-58-48.png"){
  library(ggplot2)
  
  doPlot <- c(
      "data/colorHistOriginal16Buckets.rda"
    , "data/colorHistOriginal255Buckets.rda"
    , "data/colorHistOriginalEqual16Buckets.rda"
    , "data/colorHistOriginalEqual255Buckets.rda"
    , "data/colorHistOriginalEqualRGBNorm16Buckets.rda"
    # , "data/colorHistOriginalRGBNorm16Buckets.rda"
    # , "data/colorHistOriginalRGBNorm255Buckets.rda"
    )
  
  bin <- sapply(doPlot, function(rdaData){
    load(rdaData)
    bucks <- 1:buckets
    
    data <- colorHist[1, 1:buckets]
    dfRed <- data.frame(red = bucks, data)
    data <- colorHist[1, (buckets+1):(buckets*2)]
    dfGreen <- data.frame(green = bucks, data)
    data <- colorHist[1, (buckets*2+1):(buckets*3)]
    dfBlue <- data.frame(blue = bucks, data)
    
    # hard coded scaling borders to make changes of normalization recognisable
    highest <- ifelse(gregexpr(pattern ='255', rdaData)[[1]][1] == -1, 60000, 5000)
    
    ggplot(dfRed, aes(red, data)) + geom_bar(stat = "identity", fill = "#FF0000") + 
      scale_y_continuous(limits = c(0, highest))
    ggsave(paste0("plots/", 
                  substr(rdaData, gregexpr(pattern ='/',rdaData)[[1]][1] +1, nchar(rdaData)-4),
                  "_red for ", picture, ".png"))
    ggplot(dfGreen, aes(green, data)) + geom_bar(stat = "identity", fill = "#00FF00") +
      scale_y_continuous(limits = c(0, highest))
    ggsave(paste0("plots/", 
                  substr(rdaData, gregexpr(pattern ='/',rdaData)[[1]][1] +1, nchar(rdaData)-4),
                  "_green for ", picture, ".png"))
    ggplot(dfBlue, aes(blue, data)) + geom_bar(stat = "identity", fill = "#0000FF") + 
      scale_y_continuous(limits = c(0, highest))
    ggsave(paste0("plots/", 
                  substr(rdaData, gregexpr(pattern ='/',rdaData)[[1]][1] +1, nchar(rdaData)-4),
                  "_blue for ", picture, ".png"))
  })
}


#' @title Feature Extraction - Step 2 (Histogram of Oriented Gradients)
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#' Set working directory to the package before using this function (setwd())
#' @param cells A number that describes the hog folding and by that into how many \code{cells*cells} the image will be cutted
#' @param orientations A number that describes how many orientation vectors are stored for each hog cell. This number is for the vectors from 1?-180?
#' @return A dataframe that holds the hog feature data for the images in the folder \code{"data-raw/IMG/CS CZ/normal"} of the package
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
    load(fileName)
    # namePos <- gregexpr(fileFolderName,fileName)
    # name <- substr(fileName, attr(namePos[[1]], "match.length") + 1, nchar(fileName) - 4)
    if(which(fileNames == fileName) == 1){
      hogData <- hog$hog
      rownames(hogData) <- hog$files
    } else {
      currHogRowNames <- rownames(hogData)
      hogData <- rbind(hogData, hog$hog)
      rownames(hogData) <- c(currHogRowNames, hog$files)
    }
  }
  save(hogData, file = paste(fileFolderName, "complete.Rda", sep = ""))
}

#' @title Feature Extraction - display HOG (Histogram of Oriented Gradients)
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#' @param imgPath A string that holds a path to the image that should be visualized
#' @param imgName A string that holds the name of the image (the last part of the path)
#' @param hogPath A string that holds a path to the hog feature data
#' @param cells A number that describes the hog folding and by that into how many \code{cells*cells} the image will be rastered
#' @param orientations A number that describes how many orientation vectors are stored for each hog cell. This number is for the vectors from 1?-180?
#' @examples \code{c.e.displayHogFeature("C:/Users/t_tobo01/Git/DA2-17/data-raw/IMG/CS CZ/normal/Vit/hl 2017-06-14 18-06-49-48.png", 
#' "hl 2017-06-14 18-06-49-48.png", "data/hog_original_12_9_complete.Rda", 12, 9)}
#' @author Tassilo Tobollik
c.e.displayHogFeature <- function(imgPath, imgName, hogPath, cells, orientations){
  
  image <- readPNG(imgPath)
  z.f.displayRgbImage(image=image)
  load(hogPath)
  imgNum <- which(rownames(hogData) == imgName)
  hogFeature <- hogData[imgNum,]
  
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
      #Add vector lengths for 181-360? which are the same
      lengths <- c(lengths,lengths)
      lengths <- lengths*(10^2 * 5)#lengths*(10^floor(cells/4) * 5)
      vectorValues <- cbind(degrees,lengths)
      c.f.plotHogCellVectors(vectorValues,cellCol,cellRow)
    })
  })
  return(TRUE)
}

#' @title Feature Extraction - plot HOG (Histogram of Oriented Gradients) cell vectors
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'  Function that plots vectors with given angles and length into a specified layout cell
#' @param vectorValues A vector which describes the lengths of the vectors in the cell
#' @param cellCol A number that describes the column of the rastered image in which the cell lies
#' @param cellRow A number that describes the row of the rastered image in which the cell lies
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

#' @title Feature Extraction - 
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'  
#' ...
#'
#' @author Vitali Friesen
c.g.getPixelInfo <- function(){
  #source("http://bioconductor.org/biocLite.R")
  #biocLite("EBImage")
  library(EBImage)
  # load image list
  folder <- (
    #"data-raw/IMG/CS CZ original/normal/"
    #"data-raw/IMG/CS CZ original/histEqual/"
    #"data-raw/IMG/CS CZ original/rgbNorm/"
    #"data-raw/IMG/CS CZ halved/normal/"
    #"data-raw/IMG/CS CZ quarter/normal/"
    "data-raw/IMG/CS CZ eighth/normal/"
    #"C:/Users/Nils/sciebo/DA2/Images/CS CZ sqared 28/"
  )
  
  imgList <- list.files(folder, full.names = T, ignore.case = F, recursive = T)
  # calculate for each image
  pixelFeatureMatrixEighths <- t(sapply(imgList, function(imgPath){
    # load image information into curImg
    #as.vector(channel(readImage(imgPath), "gray"))
    as.vector(readPNG(imgPath))
    
  }))
  # cut the path from the row names
  rownames(pixelFeatureMatrixEighths) <- substr(rownames(pixelFeatureMatrixEighths),
                                         nchar(rownames(pixelFeatureMatrixEighths))-28,
                                         nchar(rownames(pixelFeatureMatrixEighths)))
  
  # sort rows by their row names
  pixelFeatureMatrixEighths <- 
    pixelFeatureMatrixEighths[ order(row.names(pixelFeatureMatrixEighths)), ]
  
  save(pixelFeatureMatrixEighths, file = "data/pixelFeatureMatrixEighths.rda")
}
