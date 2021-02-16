
# Author: Ryan McCarley
# Last Updated: February 16, 2021

# About:
# This is a function for applying the modelling results to the polygons
# quadShp is the quadrangle to process, provide full path
# lassoModel is the .rds file for a specific species model, provide full path
# overwrite can be used to write out a new set of polygons with the species prediction (default is to overwrite the input shp)
#   if writing to new location, specify directory only, file name is set by input 

ApplySDMtoPolygons <- function(quadShp, 
                               lassoModel,
                               overwrite=T){
  
  # install packages if needed
  is_inst <- function(pkg){ nzchar(system.file(package=pkg)) }
  rPack <- c('sf','dplyr','glmnet')
  lapply(rPack, function(x){
    if (!is_inst(x)){ install.packages(x) }
    require(x, character.only=T)
  })
  
  # verify inputs are correctly formatTed - or throw errors
  if (!file.exists(quadShp)){
    stop(paste0('Could not establish a valid connection for ',quadShp))
  }
  if (!file.exists(lassoModel)){
    stop(paste0('Could not establish a valid connection for ',lassoModel))
  }
  if (overwrite==F){
    stop('To overwrite input polygon use overwrite=T (default), otherwise specifiy a valid output directory')
  }
  if (class(overwrite)!='logical'){
    if(!dir.exists(overwrite)){
      stop(paste0('Could not establish a valid connection for directory ',overwrite))
    }
  }

  # read polygons
  quadNm <- gsub(pattern='.shp', replacement='', x=basename(quadShp))
  fc <- sf::st_read(dsn=quadShp)
  
  # read model
  lasso <- readRDS(lassoModel)
  species <- gsub(pattern='.rds', replacement='', x=basename(lassoModel))
  
  # get desired variables from shp
  Vars <- lasso$glmnet.fit$beta %>% row.names(.) # differs depending on if using distal-proximal or priximal only
  Z <- as.matrix(dplyr::select(fc %>% sf::st_drop_geometry(), one_of(Vars)))
  
  # predict and apply
  p <- predict(lasso, newx=Z, s="lambda.min", type="response")
  fc['prob'] <- round(as.vector(p)*100,2)
  
  # get rid of prediction for developed / ag / and barren areas - less reliable areas
  # block this code out to get predictions for all polygons - just realize they might be highly inaccurate
  fc <- fc %>%
    dplyr::mutate(prob = ifelse((!is.na(aws)|aws<12) & 
                                  (dev!=21|dev!=22|dev!=23|dev!=24) & 
                                  (nass==112|nass==131|nass==141|nass==142|nass==143|nass==152|nass==176|nass==190|nass==195) & 
                                  dev!=31,
                                prob, NA))
  # block end
  
  # delete any instances of species name already in the shp
  if (species %in% names(fc)){ fc <- fc[,-which(names(fc)==species)] }
  
  # rename column
  names(fc)[which(names(fc)=='prob')] <- species
  
  # write results
  if (overwrite==T){ 
    sf::st_write(obj=fc, dsn=quadShp, delete_dsn=T)
  } else {
    if (file.exists(paste0(overwrite,'/',basename(quadShp)))){
      sf::st_write(obj=fc, dsn=paste0(overwrite,'/',basename(quadShp)), delete_dsn=T)
    } else {
      sf::st_write(obj=fc, dsn=paste0(overwrite,'/',basename(quadShp)))
      }
    }
}
