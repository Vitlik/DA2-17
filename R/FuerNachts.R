c.b.colorHist <- function(){
  library(png)
  
  buckets <- 255
  
  # load image list
  folder <- (
    "data-raw/IMG/CS CZ original/rgbNorm/"
    #"data-raw/IMG/CS CZ original/rgbNormHistEqual/"
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
  save(colorHist, buckets, file = "data/colorHistOriginalRGBNorm255Buckets.rda")
  #save(colorHist, buckets, file = "data/colorHistOriginalEqualRGBNorm255Buckets.rda")
  
  #########################################################
  # Second Feature Vector
  
  # load image list
  folder <- (
    #"data-raw/IMG/CS CZ original/rgbNorm/"
    "data-raw/IMG/CS CZ original/rgbNormHistEqual/"
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
  #save(colorHist, buckets, file = "data/colorHistOriginalRGBNorm255Buckets.rda")
  save(colorHist, buckets, file = "data/colorHistOriginalEqualRGBNorm255Buckets.rda")
}