
#sf_object is a map of Oahu (polygon sf) with no assigned crs. We're going to cycle through a bunch of crs assignments to see if any work when reprojected into WGS84

survey_grid_ncrmp.t

epsg2try=c(32601:32606,3560:3566,6620:6640,3759:3761)
crsdesdf=NULL
crsbbdf=NULL


for (i_epsg in 1:length(epsg2try)){
  utm_proj="+proj=utm +zone=4 +datum=WGS84 +units=km +no_defs +type=crs"#paste0("epsg:",epsg2try[i_epsg]," +units=km")
  crsdesdf=rbind(crsdesdf,terra::crs(utm_proj, describe=TRUE))    

  newp=sf_object
  
  st_crs(newp)=utm_proj#epsg2try[i_epsg]
  this_row=c(utm_proj,as.vector(st_bbox(st_transform(newp,crs=4326))))
  names(this_row)=c("EPSG","LON_MIN","LAT_MIN","LON_MAX","LAT_MAX")
  this_row=as.data.frame(t(this_row))
  this_row[,2:5]=as.numeric(this_row[,2:5])
  this_row$LON_DELTA=this_row$LON_MAX-this_row$LON_MIN
  this_row$LAT_DELTA=this_row$LAT_MAX-this_row$LAT_MIN
  crsbbdf=rbind(crsbbdf,this_row)
  print(i_epsg)
}
crsdf=cbind(crsbbdf,crsdesdf)


plot(crsdf$LAT_DELTA)
plot(crsdf$LON_DELTA)

 plot(newp)
 mapview(newp)
 