library(imager)
dog <- load.image("dog.jpeg")



#  Write R code that takes an image and produces a mirrored image.


mirrror <- function(img){
  ro <- array(0 , dim = dim(img))
  for ( i in 1:dim(img)[1]){
    for (j in 1:dim(img)[2]){
      ro[i,j,1,] <- img[dim(img)[1]-i+1,j,1,]
    }
  }
  return(as.cimg(ro))
}
par(mfrow = c(1,2))
rot <- mirrror(dog)
plot(rot)
plot(dog)



# Write R code that replaces all color vectors less than (.10, .10, .10) with (0, 0, 0).
img <- load.image("dog.jpeg")


for ( i in 1:dim(img)[1]){
  for (j in 1:dim(img)[2]){
    if ( img[i ,j ,1, ] <= c(.10, .10, .10)){
      img[i,j,1,] <- c(0,0,0)
    }
  }
}
plot(img)
plot(dog)




# Write a function prop.color that calculates the proportion of pixels in a given image that are within
# a 0.5 Euclidean distance from a given color. That is, if ???????????? is the rbg vector of the ????, ????th pixel and ???? is
# a given color vector, then function should return the proportion of pixels in the image for which
# ??????????????? ??? ??????? ??? 0.5
# (Here norm is the 2-norm or the Euclidean norm).
# The function should take two arguments; img which will be an imager image and col which will take
# a vector of length 3




prop.color <- function(img , col ){
  count <- 0
  for ( i in 1:dim(img)[1]){
    for (i in 1:dim(img)[2]){
      if ( norm(img[i,i,1,] - col ,"2") <= 0.5){
        count <- count + 1
      }
    }
  }
  return(count/{dim(img)[1]*dim(img)[2]})
}
col <- c(0.3 , 0.3 , 0.3)
prop.color(dog, col)







