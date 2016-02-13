#class balancer
ClassBalance <- function(dataframe, targetClass, targetQty){
  obs <- as.vector(which(dataframe[,ncol(dataframe)]==targetClass))
  universe <- dataframe[obs,]
  maxObs<-max(table(dataframe[,ncol(dataframe)]))
  sampleQty <-floor(maxObs*targetQty)
  
  
  vectorClass<-universe[sample(nrow(universe), sampleQty, replace=TRUE),]
  
  combined<-rbind(dataframe, vectorClass)
  combined <- combined[sample(1:nrow(combined)), ]
  
  return(combined)
}

#normalizer
normalize <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))}
