
options(scipen=999)
library(sf)
library(rmapshaper)
library(dplyr)
library(raster)
library(fasterize)

# idaho boundary ----
idaho <- sf::st_read('F:/idahoveg/arcmap/ecosections/idaho_wgs84.shp') %>% 
  rmapshaper::ms_simplify(., keep_shapes=T)
saveRDS(idaho, 'data/idaho.rds')

# get file size
l <- list.files('F:/OneDrive - University of Idaho/IdahoVegRepo/Data/IdahoPolygons', full.names=T)
zipSize <- round(file.size(l)*9.537e-7, 3)
l2 <- list.files('F:/idahoveg/idahoveg_polygons/polygons_081519', full.names=T)
rawSize <- round(file.size(l2)*9.537e-7, 3)
dfQuadAttr <- full_join(data.frame(q=basename(l) %>% gsub('.zip','',.) %>% gsub('^q','',.), Mb_zip=zipSize),
                        data.frame(qraw=sub(pattern="(.*?)\\..*$", replacement ="\\1", basename(l2)), Mbraw=rawSize) %>% 
                          mutate(q=gsub('^q','',qraw)) %>% 
                          group_by(q) %>% 
                          summarise(Mb_shp=sum(Mbraw, na.rm=T)),
                        by='q')
# add web destination


# 24k quads ----
# eliminate quads outside idaho
quads <- sf::st_read('F:/idahoveg/idahoveg_usgs_quads/quadbnd24k_id_igdc/quad24k_proj.shp')
i <- list.files('F:/idahoveg/idahoveg_archive/idaho_habitat_patches') %>% gsub('.zip','',.) %>% gsub('^q','',.)
quads <- quads[which(quads$UID %in% i),]
# write out version for archive
st_write(obj=quads, dsn='F:/idahoveg/idahoveg_usgs_quads/quadbnd24k_id_igdc/USGS24k.shp', delete_dsn=T)
# set up for shiny
quads <- sf::st_read('F:/idahoveg/idahoveg_usgs_quads/quadbnd24k_id_igdc/USGS24k.shp') %>% 
  left_join(., dfQuadAttr, by=c('UID'='q')) %>% 
  sf::st_transform(x=., crs=st_crs(4326)) %>% 
  rmapshaper::ms_simplify(., keep=1, keep_shapes=T)
saveRDS(quads, 'data/quads.rds')

# rinker rock creek ranch ----
rrcr <- sf::st_read('F:/IdahoVeg/rock_creek/shps/rock_creek_ranch.shp') %>% 
  rmapshaper::ms_simplify(., keep=1, keep_shapes=T)
saveRDS(rrcr, 'data/rrcr.rds')

# rinker rock creek ranch polygons ----
polygons <- sf::st_read('F:/IdahoVeg/rock_creek/shps/polygons_dp_rcc.shp')

# make raster of ids
ras <- fasterize(polygons, raster=raster(polygons, res=1), field='id')
writeRaster(ras, 'data/test.tif')
  sf::st_transform(x=., crs=st_crs(4326)) %>% 
  rmapshaper::ms_simplify(., keep=1, keep_shapes=T)

# table
polyTBL <- polygons %>% st_drop_geometry()
saveRDS(polyTBL, 'data/polyTBL.rds')

# polygons <- polygons %>%
#   mutate(rid=as.character(id))

saveRDS(polygons, 'data/polygons.rds')

ras <- fasterize(polygons, raster=raster(polygons, res=1), field='id')


polyTBL <- polygons %>% st_drop_geometry()
saveRDS(polyTBL, 'data/polyTBL.rds')
