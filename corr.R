corr <- function(directory, threshold = 0){
      id <- 1:332
      idvect <- integer(length(id))
      final <- data.frame()
      filecom <- data.frame()
      ok <- data.frame()
      data2 <- numeric()
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
            cleaner <- file[,2:3]
            cc <- complete.cases(cleaner)
            nobs<- sum(cc)
            
            
            if(nobs > threshold){
                  
                  filecom <- data.frame(file)
                  nitrate <- filecom[3]
                  sulfate <- filecom[2]
                  df <- cor(nitrate,sulfate,use="complete.obs")
                  data <- c(df)
                  data2 <- append(data2,data)
                  ##print(cases)
                 ## print (ok)
                  
            }
            
      }
      data2
}