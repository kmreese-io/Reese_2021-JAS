## SPATIAL POLYGON ##
## R. KYLE BOCINSKY

polygonUTM_NAD83 <- function(UTM_West,UTM_East,UTM_South,UTM_North,UTM_Zone) {
  datainUTM<-matrix(c(UTM_East,UTM_West,UTM_West,UTM_East,UTM_East,UTM_North,UTM_North,UTM_South,UTM_South,UTM_North),nrow=5)
  project <- CRS(paste('+proj=utm +datum=NAD83 +zone=',UTM_Zone,sep=''))
  sim.poly <- Polygons(list(Polygon(datainUTM,hole=FALSE)),ID='A')
  sim.poly <- SpatialPolygons(list(sim.poly),proj4string=project)
  IDs <- sapply(slot(sim.poly,'polygons'),function(x) slot(x,'ID'))
  df <- data.frame(rep(0,length(IDs)),row.names=IDs)
  sim.poly <- SpatialPolygonsDataFrame(sim.poly,df)
  return(sim.poly)
}