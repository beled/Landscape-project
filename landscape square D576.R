library(landscapeR)
library(raster)
library(sp)
library(pracma)

#-------------------------------------------------
#Note : If you want to change matrix size, change tm
#       If you want to change the number of patches, change nf.
#       If you want to change buffer size, change buffer
#       Af should remain the same between each disposition
#       Change Name to change the directory and names of files created by this program

#nf represent nothing in this code but allow it to be consistent with other disposition
nf <- 30

#Distribute the patches in the landscape
Af <- nf*3000  #Total area

###Creating a rasterLayer
#Starting values
tm <- 576       #Matrix size
n <- 32      #One color square size
nct <- (tm/n)^2/2 #Number of color squares 
buffer <- 32  #Note : always buffer >= n, if not error
#Producing the background matrix and the raster
m <- matrix(0, tm, tm)  #background matrix
for (j in 1:(tm/n)) { #This loop creates columns
  if (j %% 2==1) {  #Because color squares alternate, they are not identically dispose on each column
    for (i in 1:(tm/(2*n))){  #This loop creates lines
      m[c((2*n*i-(2*n-1)):(2*n*i-n)),c(((j-1)*n+1):(j*n))]<- i+(j-1)*(tm/(2*n))   
    } 
  } else {
      for (i in 1:(tm/(2*n))){
        m[c((2*n*i-(n-1)):(2*n*i)),c(((j-1)*n+1):(j*n))]<- i+(j-1)*(tm/(2*n))   
      } 
  }
}
rr <- raster(m, xmn=0, xmx=tm, ymn=0, ymx=tm)   #rr is the raster 
ver <- as.matrix(rr)

#Remove patches to obtain approximately Af
si <- 0
while (si==0) {
  i <- 1
  nc <- round((tm^2/2-Af)/n^2)  #number of squares to remove base on matrix size and square size
  moins <- round(runif(nc,1,nct))  #Create a list of values to be remove from rr
  while (i<=nc) {
    if (sum(moins[i]==moins)>1) {   #Ensure me that each value to be remove are distinct
      moins[i]<-round(runif(1,1,nct))    
    } else {
      ver[ver==moins[i]]<-0   #remove the square associated to the value
      i<-i+1
      }
  }
  #Calculate the percentage of area cover in the buffer relatively to Af
  liste <- c(1:(tm/(2*n)*buffer/n),(nct+1-(tm/(2*n)*buffer/n)):(nct-1), seq((1+tm/n),nct,(tm*16/(tm-2*buffer))),seq((tm/n),nct,(tm*16/(tm-2*buffer))))  #Create a list of squares in the buffer
  compt <- c()
  for (i in 1:length(liste)) {
    compt <- c(compt,sum(moins==liste[i]))
  }
  perc <- (length(liste)-sum(compt))/(nct-nc)*100
  #If there are too many squares in the buffer, restart.
  if (perc<21){ 
    si <- 1
  }
}


#------------------------------------
#Change Name to change the directory and names of files created by this program
Name <- paste('C:/Users/Benjamin/Documents/R/R landscape project/Landscape project Ben/Square_patch_D', tm, '/square_patch_D',tm,'_n',n,sep='')

#Save the final matrix
#write.table(ver, paste(Name, '.txt',sep = ''), row.names=FALSE, col.names = FALSE, sep=", ",qmethod="double")
