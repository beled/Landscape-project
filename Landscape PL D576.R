library(landscapeR)
library(raster)
library(sp)
library(pracma)


#-------------------------------------------------
#Note : If you want to change matrix size, change tm
#       If you want to change the number of patches, change nf
#       Af should remain the same between each disposition
#       Change Name to change the directory and names of files created by this program

#Generate N patches with area power law distributed

#set seed
for (z in 1:50) {   #Allow me to create various random disposition
  Seed <- z+99;
  set.seed(Seed)
  
  # mean of distribution
  mu <- 3000;
  #Number of patches 
  nf <- 30;
  
  #The sum of all fragments area 
  Af <- mu*nf;
  
  #Parameters of the PL
  p <-  0.99;
  d <- mu*(1-p);
  
  
  while(1){
    
    while(1){
      aff <- d*(rand(nf,1))^(-p)
      af <- round(aff/sum(aff)*Af);       #This line and the following insure that the sum of all fragments equal Af
      af[1] <- af[1] - sum(af) + Af;
      if(max(af)>45000)
        break
    }
    test <- sum(af>0)  #If not all entries are non-zero, repeat
    if(test==nf)
      break
  }
  
  #Rank by decreasing order
  af <- af[order(-af)]
  #Create a vector of mean radius for each patch to be use later
  rayon <- sqrt(af/pi)
  
  #-------------------------------------------
  #Distribute the patches in the landscape
  si <- 0
  while (si == 0) {   #the while loop restart if af is too different from the patches placed in the landscape
    #Creating a rasterLayer
    tm <- 576   #matrice size
    buffer <- 32   #buffer size
    m <- matrix(0, tm, tm)
    rr <- raster(m, xmn=0, xmx=tm, ymn=0, ymx=tm)   #Note that rr is the only raster layer (only containing only zeros for the moment)
    
    
    ###How does it work?
    #1. Create a matrix position where there are one points per patches. 
    #     These are the initial points where the patches will be built on.
    #     This matrix is not the raster rr or the bakcground matrix m.
    #2. Base on the matrix position, create patches in the raster rr.
    ###
    
    #1. Creating a matrix position for the patches
    position <-c(tm/2,tm/2) #Creating a point in the center of the matrix
    position <- rbind(position,round(runif(2,min = 1+buffer, max = tm-buffer)))
    i <- 2
    compte <- 0
    while (i<=nf){    #If there is not enough place between two points, recreate the point.
      dist <- c()
      distmin <- c()
      for (j in 1:(i-1)) {
        dist <- c(dist,sqrt((position[i,1]-position[(i-j),1])^2+(position[i,2]-position[(i-j),2])^2)) #The real distance between two points
        distmin <- c(distmin,10+rayon[i]/5+rayon[i]+rayon[i-j])  #The theorical minimum distance between two points.
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
    
    #---------------------------------------
    #Verify if the patches cover the same area as at the start
    ver<- as.matrix(rr)
    vf <- c()
    comp <- c()
    for (i in 1:nf) {
      vf <- c(vf, sum(ver==i))
      if (abs(af[i]-vf[i])/af[i]<=0.015) {  #Uncertainty is 1%
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
  #------------------------------------
  #Change Name to change the directory and names of files created by this program
  Name <- paste('C:/Users/Benjamin/Documents/R/R landscape project/Landscape project Ben/PL_patch_D', tm ,'/pL_patch_D', tm, '_',Seed,sep='')
  
  #Save the final matrix
  write.table(ver, paste(Name, '.txt',sep = ''), row.names=FALSE, col.names = FALSE, sep=", ",qmethod="double")
  write.table(af, paste(Name, '_af.txt',sep = ''), row.names=FALSE, col.names = FALSE, sep=", ",qmethod="double")
  #Note : Because I was unable to make an histogram with the power law, af is also saved with the final matrix.
}  