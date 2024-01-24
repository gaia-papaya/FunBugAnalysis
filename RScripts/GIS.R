#THIS IS THE SCRIPT TO ANALYZE GEOPSAPTIAL VARIABLES AND IMPORT INTO DATASET. THIS SCRIPT IS MEANT TO RUN <FIFTH> IN THE SCRIPT SEQUENCE
require(rgdal)
require(sf)
require(terra)

#export bounding box to deal w clipping and reprojecting in QGIS (unless it exists already)
bounding_box <- st_as_sf(as(raster::extent(-74.35, -72.88,40.48,42.17), "SpatialPolygons")) %>% 
  st_set_crs(4326) %>% st_transform(6347)
#st_write(bounding_box, "QOutput/bounding_box.shp", append = F)

#RASTERS----
#read in raster data (already clipped and reprojected in QGIS)
ISC <- raster::raster("QOutput/clipped_ISC2021.tif")
CC <- raster::raster("QOutput/clipped_CC2021.tif")
ALAN <- raster::raster("QOutput/aligned_clipped_ALAN.tif")
YrTemp <- raster::raster("QOutput/clipped_TempAnnum.tif") 
crs(YrTemp) <- crs(ALAN)
YrPrecip <- raster::raster("QOutput/clipped_PrecipAnnum.tif")
crs(YrPrecip) <- crs(ALAN)

#SHAPEFILES----
#civil boundaries of NY state for ez masking of rasters 
NY_County_Shorelines <- st_read("QLayers/NYS_Civil_Boundaries.shp/Counties_Shoreline.shp")  #NAD83

#shapefile of city parks proprieties and fix CRS bc NY govt cant fucking format their data right
CityPark_Shapes <- st_read("QLayers/Parks Properties") %>% #WGS84
  st_set_crs(4326) %>% 
  st_make_valid() %>% 
  st_transform(crs = crs(NY_County_Shorelines)) #reproject to NAD83

#reformat wide veg datatable to be sf object, set crs (WGS84 from iPhone 11 and Columbus P-7 GNSS receivers used)
VegData_W_sf <- st_as_sf(select(Veg_Data_wide, LongitudeDec, LatitudeDec, SiteRep), coords = c("LongitudeDec", "LatitudeDec")) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = crs(NY_County_Shorelines)) #reproject to NAD83

#filter out to just parks and counties that contain my sites
CityPark_Shapes <- CityPark_Shapes[VegData_W_sf,]
NY_County_Shorelines <- NY_County_Shorelines[VegData_W_sf,]
#export
#st_write(CityPark_Shapes, "QOutput/CitySites.shp")
#st_write(NY_County_Shorelines, "QOutput/CountySites.shp")

#create  1km buffers around points
VegBuffer <- st_buffer(VegData_W_sf, 1000)

#calculate zonal stats for ISC, CC< and ALAN in the 1km buffers
ISC_zs<- terra::extract(terra::rast(ISC), terra::vect(VegBuffer), fun = mean, bind = T) %>% 
  as.data.frame() 
ALAN_zs<- terra::extract(terra::rast(ALAN), terra::vect(VegBuffer), fun = mean, bind = T) %>% 
  as.data.frame() 
CC_zs<- terra::extract(terra::rast(CC), terra::vect(VegBuffer), fun = mean, bind = T)%>% 
  as.data.frame() 
YrTemp_zs<- terra::extract(terra::rast(YrTemp), terra::vect(VegBuffer), fun = mean, bind = T)%>% 
  as.data.frame()
YrPrecip_zs<- terra::extract(terra::rast(YrPrecip), terra::vect(VegBuffer), fun = mean, bind = T)%>% 
  as.data.frame()

#merge zstats into veg data sf object
GIS_Data <- data.table::as.data.table(VegData_W_sf) %>% 
  left_join(.,ISC_zs, by = "SiteRep") %>% 
  left_join(., CC_zs, by = "SiteRep") %>% 
  left_join(., ALAN_zs, by = "SiteRep") %>% 
  left_join(., YrPrecip_zs, by = "SiteRep") %>% 
  left_join(., YrTemp_zs, by = "SiteRep") %>% 
  rename(AVG1km_ALAN = aligned_clipped_ALAN_1,
         AVG1km_ISC = clipped_ISC2021,
         AVG1km_CC = clipped_CC2021,
         MeanAnnumPrecip = clipped_PrecipAnnum,
         MeanAnnumTemp = clipped_TempAnnum) 

#remove intermediary objects
rm(ALAN, ISC, CC, bounding_box, CC_zs, ISC_zs, ALAN_zs, VegBuffer, VegData_W_sf, YrTemp, YrTemp_zs, YrPrecip_zs, YrPrecip)
