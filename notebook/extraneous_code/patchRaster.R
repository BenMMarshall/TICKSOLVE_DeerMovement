### Function to reclassify raster to focal land use type and calculate areas of patches #####
### Inputs ####

#class_raster - raster of land use classes

#rclmat - matrix passed to terra classify function two columns (existing value, new value)

#sizethresh-select patches larger than this threshold (in m2) ## crs unit must be in meters. 

#polyregion - polygon of regions of interest if included rater will be cropped to region

## Output is a list 

#[[1]] raster with patches and ID, if size threshold included then size threshold is applied
#[[2]] dataframe with patch ID and sizes and which patches were selected if size threshold is applied
#[[3]] binary raster, if size threshold then size threshold is applied


patchRaster <- function(class_raster=NULL,rclmat=NULL,sizethresh=NULL,polyregion=NULL){
  require(terra)
  require(mgc)
  if(!is.null(polyregion)){
   class_raster <- mask(crop(class_raster,extent(polyregion)),polyregion) ### mask and crop raster to classify
  }
  
  class_raster <- rast(class_raster)
  
  ## Create a binary raster for focal land class ##
  Init_ras <- classify( ### classify to focal land class
    class_raster,
    rcl=rclmat) ## classify using input
  
  sq_size_m_sq <-(res(Init_ras)[1])*(res(Init_ras)[2])
  
  ### Calculate patch area ###
  
  patch.class<- mgc::ConnCompLabel(as.matrix(Init_ras,wide=TRUE))
  
  Patch.classify <- as.data.frame(table(patch.class),stringsAsFactors=FALSE)
  
  colnames(Patch.classify) <- c("patch_id","num_pixels")
  
  Patch.classify <- Patch.classify[Patch.classify$patch_id!=0,] 
  
  Patch.classify$area_m_sq <- Patch.classify$num_pixels*sq_size_m_sq
  
  
  if(!is.null(sizethresh)){
  Patch.classify$Select <- ifelse(Patch.classify$area_m_sq>=sizethresh,1,0)
  }else{
    Patch.classify$Select <- 1
  }
  
  Patch.classify[, c(1:4)] <- sapply(Patch.classify[, c(1:4)], as.numeric)
  
  Patch.ras <- rast(patch.class,crs=crs(Init_ras),extent=ext(Init_ras))
  
  if(!is.null(sizethresh)){
  Bin.ras.final <- classify(Patch.ras,rcl=Patch.classify[,c(1,4)]) ## remove patches below size threshold
  Patch.ras[Bin.ras.final==0] <- 0
  } else {
  Bin.ras.final <- Init_ras
  }
  
  return(list(Patch_ras=Patch.ras,Patch_area=Patch.classify,Binary_ras=Bin.ras.final))
  
}
