library(landscapeR)
library(raster)
library(sp)
library(pracma)

#-------------------------------------------------
#Note : If you want to change matrix size, change tm
#       If you want to change the number of columns, change n.
#       Af should remain the same between each disposition
#       Change Name to change the directory and names of files created by this program

#set seed
for (z in 1:25) {   #Allow me to create various random disposition
  Seed <- z+99;
  set.seed(Seed)
  
  #-----------------------------------------------
  #Distribute the patches in the landscape
  Af <- 90000
  si <- 0
  while (si == 0) {   ##the while loop restart if af is too different from the patches placed in the landscape
    ###Creating a rasterLayer
    #Starting values
    tm <- 576       #Size of the matrix side
    n <- 8      #Number of lines
    
    #Producing the background matrix and the raster
    m <- matrix(0, tm, tm)      #m is the background matrix
    for (i in 1:n){
      m[,tm/(2*n)*(2*i-1)]<- i   #Size of each column
    }
    rr <- raster(m, xmn=0, xmx=tm, ymn=0, ymx=tm)   #rr is the raster 
    
    #Creating patches
    place <- Af-tm*n       #place is the remaining space to be cover
    for (i in 1:n) {
      rr <- expandClass(rr, i, place/n)     
    }
    
    #Verify if the patches cover the same area as the starting one
    ver<- as.matrix(rr)
    if (abs(1-sum(ver>0)/Af)<=0.0005) {        #uncertainty is 0.05%
      print("Les données sont vérifiées")
      si <- 1
    } else {
      print("Les donnees ne sont pas verifies")
    }
  }
  
  #----------------------------------------
  #Change Name to change the directory and names of files created by this program
  Name <- paste('C:/Users/Benjamin/Documents/R/R landscape project/Landscape project/Linear_patch_D288/linear_patch_D', tm, 'n',n,'_',Seed,sep='')
  #Write a file containing the final matrix
  write.table(ver, paste(Name, '.txt',sep = ''), row.names=FALSE, col.names = FALSE, sep=", ",qmethod="double")
}  