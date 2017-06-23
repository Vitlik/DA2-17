#Takes approx. 1.1294 secs per screenshot
rgbNorm <- function(filename, x = 480, y = 640, z = 3){
  pos <- gregexpr(".png", filename)
  filename2 <- substr(filename, 1, (pos[[1]]-1))
  
  img = readPNG(filename)
  
  # Compute the sums of the pixel's values over all channels
  S <- apply(img,c(1,2),sum)
  
  normImg <- array(seq(0,0), dim = c(x,y,z))
  # Compute the normalized RGB values by dividing each pixel's value in each dimension by S
  #normImg[,,1] <- img[,,1]/S
  #normImg[,,2] <- img[,,2]/S
  #normImg[,,3] <- img[,,3]/S
  normV <- sapply(1:z,function(x) img[,,x]/S)
  normImg <- array(normV, dim = c(x,y,z))
  # Set NaN values, received from dividing 0 by something, to 0
  normImg[is.nan(normImg)] = 0
  
  writePNG(normImg,paste(filename2,"Norm.png", sep = ""))
  return(normImg)
}
