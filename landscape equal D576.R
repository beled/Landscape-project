library(landscapeR)
library(raster)
library(sp)
library(pracma)

#-------------------------------------------------
#Note : If you want to change matrix size, change tm
#       If you want to change the number of patches, change nf.
#       Af should remain the same between each disposition
#       Change Name to change the directory and names of files created by this program


#Generate N patches with a equal distribution

#set seed
for (z in 1:50) {   #Allow me to create various random disposition
  Seed = z+99;
  set.seed(Seed)
  
  #Number of patches 
  nf <- 60;
  
  #Create af
  rayon <- sqrt(1500/pi)
  af <- rep(1500, times=nf)
  #-------------------------------------------
  #Distribute the patches in the landscape
  si <- 0
  while (si == 0) {   #the while loop restart if af is too different from the patches placed in the landscape
    #Creating a rasterLayer
    tm <- 576 #matrice size
    buffer <- 32  #buffer size
    m <- matrix(0, tm, tm)
    rr <- raster(m, xmn=0, xmx=tm, ymn=0, ymx=tm)   #Note that rr is the only raster layer (only containing zeros for the moment)
    
    
    ###How does it work?
    #1. Create a matrix position where there are one points per patches. 
    #     These are the initial points where the patches will be built on.
    #     This matrix is not the raster rr or the bakcground matrix m.
    #2. Base on the matrix position, create patches in the raster rr.
    ###
    
    #1. Creating a matrix position for the patches
    position <-c(tm/2,tm/2) #Create a point at the matrix m center
    position <- rbind(position,round(runif(2,min = 1+buffer, max = tm-buffer)))
    distmin <- 10+rayon+rayon  #The theorical minimum distance between two points.
    i <- 2
    while (i<=nf){    #If there is not enough place between two points, recreate the point.
      dist <- c()
      for (j in 1:(i-1)) {
        dist <- c(dist,sqrt((position[i,1]-position[(i-j),1])^2+(position[i,2]-position[(i-j),2])^2)) #The real distance between two points
      }
      if (sum(dist>=distmin)==length(dist)) {   #If dist<dist_min, restart.
        position <- rbind(position,round(runif(2,min = 1+buffer, max = tm-buffer)))
        i <- i+1
      } else {
        position[i,] <- round(runif(2,min = 1+buffer, max = tm-buffer))
      }
    }
    #2. Create patches on the raster rr
    for (i in 1:nf) {
      rr  <- makeClass(rr, 1, af[i], pts = matrix(position[i,], ncol =2), val = i, bgr=0)
    }
    #Note that all patches have a different value that allows me to differentiate one from another
    
    #----------------------------------------------------
    #Verify if the patches cover the same area as the initial ones
    ver<- as.matrix(rr)
    vf <- c()
    comp <- c()
    for (i in 1:nf) {
      vf <- c(vf, sum(ver==i))
      if (abs(af[i]-vf[i])/af[i]<=0.005) {  #Uncertainty is 0.5%
        comp[i]<-1
      } else {
        comp[i]<-0
      }
    }
    if (sum(comp)==length(comp)) {
      print("Les données sont vérifiées")
      si <- 1
    } else {
      print("Les donnees ne sont pas verifies")
    }
  }
  
  #------------------------------------------
  #Change Name to change the directory and names of files created by this program
  Name = paste('C:/Users/Benjamin/Documents/R/R landscape project/Landscape project Ben/Equal_patch_D', tm, '/equal_patch_D', tm, '_',Seed,'_n60', sep='')
  #Write a file containing the final matrix
  write.table(ver, paste(Name, '.txt',sep = ''), row.names=FALSE, col.names = FALSE, sep=", ",qmethod="double")
}