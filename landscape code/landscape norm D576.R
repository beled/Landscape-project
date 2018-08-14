.libPaths("/home/beled/R/x86_64-pc-linux-gnu-library/3.5")
library(sp)
library(pracma)
library(stats)
library(raster)
library(landscapeR)

#-------------------------------------------------
#Note : If you want to change matrix size, change tm
#       If you want to change the number of patches, change nf.
#       You will also need to change the mean and sd of normal curve
#       Af should remain the same between each disposition
#       Change Name to change the directory and names of files created by this program

#Generate N patches with a normal distribution

#Set seed
for (z in 1:50) {   #Allow me to create various random disposition 
  Seed <- z+99;
  set.seed(Seed)
  
  #Number of patches 
  nf <- 30;
  
  #Normal distribution
  i<-0
  j<-0
  Af <- nf*3000    #Total area covert by patches
  while (i == 0 || j==0) {
    af <- rnorm(n=nf, mean=3000, sd = 1500) #Create the normal distribution as a vector
    if (sum(af)!=Af) {                  #Ensure that sum(af) equal the initial area
      af <- round((af/sum(af))*Af);
      af[1] <- af[1]-(sum(af)-Af)
      j<-1
    }
    if (sum(af>0)==nf) {
      i<-1         #Ensure that all numbers are positive
    }
  }
  #Rank by decreasing order
  af <- af[order(-af)] 
  #Create a vector of mean radius for each patch to be use later
  rayon <- sqrt(af/pi)
  
  #-------------------------------------------
  #Distribute the patches in the landscape
  verify_af <- 0
  while (verify_af == 0) {   #the while loop restart if af is too different from the patches placed in the landscape
    #Creating a rasterLayer
    tm <- 576   #matrice size
    buffer <- 32    #buffer size
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
        distmin <- c(distmin,10+rayon[i]+rayon[i-j])  #The theorical minimum distance between two points.
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
    #Verify if the patches cover the same area as the initial ones
    ver<- as.matrix(rr)
    vf <- c()
    comp <- c()
    for (i in 1:nf) {
      vf <- c(vf, sum(ver==i))
      if (abs(af[i]-vf[i])<=2) {  
        comp[i]<-1
      } else {
        comp[i]<-0
      }
    }
    if (sum(comp)==length(comp)) {
      #print("Les données sont vérifiées")
      verify_af <- 1
    } else {
      #print("Les donnees ne sont pas verifies")
    }
  }
  
  #------------------------------------
  #Change Name to change the directory and names of files created by this program
  Name <- paste('/project/6004956/beled/Norm_patch_D', tm, '/norm_patch_D',tm,'_',Seed,sep='')
  #Display the result in a histogram
  png(filename = paste(Name,'.png', sep = ''))  #Open a png file
  
  h<-hist(af, freq = TRUE, breaks = seq(0,8000,400), 
          main = 'Distribution normale du territoire', xlab = 'Taille', 
          ylab = 'Nombre de territoires', xlim = c(0, 8000), ylim = c(0, 8), col = 'grey')
  
  xfit <- seq(0, 8000, length = 50)        
  yfit <- dnorm(xfit, mean = mean(af), sd = sd(af))     
  yfit <- yfit * diff(h$mids[1:2]) * length(af) 
  lines(xfit, yfit, col = "black", lwd = 2)
  box() #fait une boite autour du graph
  dev.off()
  
  #Save the final matrix
  write.table(ver, paste(Name, '.txt',sep = ''), row.names=FALSE, col.names = FALSE, sep=", ",qmethod="double")
}  