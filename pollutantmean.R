pollutantmean <- function(directory, pollutant, id = 1:332) {
      idvect <- integer(length(id))
      filecom <- data.frame()
      for(x in 1:length(id)) {
            
            if(id[[x]]< 10){
                  idvect[x] = paste("00",id[x], sep="")
            }
            else if(id[[x]] >= 10 & id[[x]]< 100){
                  idvect[x] = paste("0",id[x],sep="")
            }
            else{
                  idvect[x] = paste(id[x])
            }
            filename <- paste(directory,'\\',idvect[x],".csv",sep="")
     file <- data.frame(read.csv(filename,T))
     filecom <- data.frame(rbind(file,filecom))
      }
     mean <- subset(filecom,select=pollutant)
     colMeans(mean,na.rm=TRUE)
}


