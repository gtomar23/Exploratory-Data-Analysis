rotate_image <- function(image , angel){
  if (angel == 90){
    ro <- array(0,dim= c(dim(image)[2],dim(image)[1],1,3))
    for ( i in 1:dim(image)[1]){
      for (j in 1:dim(image)[2]){
        ro[j,i,1,] <- image[i,dim(image)[2] - j + 1 ,1,]
      }
    }
    plot(as.cimg(ro))
    return(ro)
  }
  else if ( angel == 180){
    ro180 <- array(0,dim= c(dim(image)))
    for ( i in 1:dim(image)[1]){
      for (j in 1:dim(image)[2]){
        ro180[i,j,1,] <- image[dim(image)[1]-i+1,dim(image)[2] - j + 1 ,1,]
      }
    }
    plot(as.cimg(ro180))
    return(ro180)
  }
  
  else  if ( angel == 270){
    ro270 <- array(0,dim=c(dim(image)[2],dim(image)[1],1,3))
    for ( i in 1:dim(image)[1]){
      for (j in 1:dim(image)[2]){
        ro270[j,i,1,] <- image[dim(image)[1]-i+1,j  ,1,]
      }
    }
    plot(as.cimg(ro270))
    return(ro270)
  }
}