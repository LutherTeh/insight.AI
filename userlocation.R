
## INSIGHT.AI Location map

userlocation <- function(data){
  
  set.seed(12345)
  
  require(ggplot2)
  require(ggmap)
  require(mapproj)
  require(sp)

  data$longitude<- runif(nrow(data),101.625,101.643)
  data$latitude<-runif(nrow(data),3.0175,3.204)
  
  coord <- cbind(data$longitude,data$latitude)
  coord2  <-as.data.frame(coord)
  colnames(coord2) <- c("longitude","latitude")
  
  
  #plot the  hybrid Google Maps basemap
  map<- get_map(location = 'Puchong', zoom = 11)

  g<- ggmap(map)+geom_point(data=coord2,alpha = .8, aes(x=longitude, y=latitude),color='red')
  g<- g +ggtitle("Maxis user location")

  return(g)
}

