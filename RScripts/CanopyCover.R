#THIS IS THE SCRIPT TO PROCESS CANOPY COVER DATA FOR EACH SITE. THIS SCRIPT IS MEANT TO RUN <Second> IN THE SCRIPT SEQUENCE
#RUNNING CODE OUT OF SEQUENCE WILL RESULT IN WORKFLOW FAILURE

#Read in datasheet
CanopyCover_RAW<- data.table::fread(file = "RData/input/CanopyCoverDatasheet.csv") 
#exclude row with lens flare (see notes in .csv)
CanopyCover_RAW<- CanopyCover_RAW[notes == ""]

#initialize constant values
TotalPixels <- 3024*4032
#calculate % canopy cover
CanopyCover_RAW[, CanopyCoverPer := (PxArea_Tree/TotalPixels)*100]

#concatenate site and rep strings for ez merging
CanopyCover_RAW[, SiteRep := paste(paste(str_sub(Site, 1L, 1L), Rep, str_sub(Site, -1L,-1L), sep = ""))]

#merge canopy cover data into Vegetation data
Veg_Data_wide<- Veg_Data_wide %>% 
  left_join(., select(CanopyCover_RAW, Site, CanopyCoverPer), by = "Site")

#remove intermediary objects
rm(TotalPixels, CanopyCover_RAW)
