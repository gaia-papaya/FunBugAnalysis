#THIS IS THE SCRIPT TO PROCESS PLANT DIVERSITY DATA. THIS SCRIPT IS MEANT TO RUN <First> IN THE SCRIPT SEQUENCE.
#RUNNING CODE OUT OF SEQUENCE WILL RESULT IN WORKFLOW FAILURE

#dependencies----
require(tidyverse)

#read tree and floor diversity tables
Plant_Data_RAW <- read.csv("RData/input/PlantDatasheet.csv")
Tree_Data_RAW <- read.csv("RData/input/TreeDatasheet.csv")

#Ground cover plant diversity----
#long table with separate cols for species name and counts
Plant_Data_long <-  Plant_Data_RAW %>%
  group_by(Site, Rep) %>% 
  mutate(PlantRichness = unique(n()), #calculate Richness from long table
         Date = as.POSIXct(Date),
         SiteRep = paste(str_sub(Site, 1L, 1L), Rep, str_sub(Site, -1L,-1L), sep = "")) %>%  #Combine Site and Replicate names as uid
  ungroup() %>% 
  arrange(SiteRep)

#pivot towide table with #species count matrix 
Plant_Data_wide <- Plant_Data_long %>% 
  group_by(Site, Rep) %>%
pivot_wider( names_from = "Species", values_from = "Count", values_fill = 0) %>% #widen to calculate diversity indices, filling NAs as 0
          
  ungroup() %>% 
  #Calculate diversity indices
  mutate(PlantShannon = vegan::diversity(select(.,"Carex pennsylvanica":"Galium circaezans")), #Shannon-Weiner Index
         PlantSimpson = vegan::diversity(select(.,"Carex pennsylvanica":"Galium circaezans"),"simpson"), #Simpson's Diversity Index
         PlantInvSimp = vegan::diversity(select(.,"Carex pennsylvanica":"Galium circaezans"),"invsimp"), #Simpson's Dominance Index
         PlantEvenness = PlantShannon/PlantRichness #Shannon Evenness 
         ) %>% #reorder the table for readability
  select(Site, Rep, Date, PlantRichness, PlantShannon, PlantSimpson, PlantInvSimp, PlantEvenness, everything()) %>% 
  arrange(SiteRep)

#merge diversity scores back into long data
Plant_Data_long <- full_join(Plant_Data_long, select(Plant_Data_wide,SiteRep,  PlantShannon:PlantEvenness), by = "SiteRep")

#remove raw data
rm(Plant_Data_RAW)

#Tree Diversity----
#long table with separate columns for species name and counts
Tree_Data_long <- Tree_Data_RAW %>% 
  group_by(Site, Rep) %>% 
  mutate(SiteRep = paste(str_sub(Site, 1L, 1L), Rep, str_sub(Site, -1L,-1L), sep = ""), #Combine Site and Replicate names as uid
         N_Trees = n(), #total # of trees in the sample
         TreeRichness = length(unique(Species)),  #Tree species richness
         Date = as.POSIXct(Date), #reformat date to datetime
         Sample_Time = difftime(as.POSIXct(Time_End, format = "%H:%M"),
                                as.POSIXct(Time_Start, format = "%H:%M"), units = "mins"), #get difference between time end and start to get sampling time
         PropTrees_Sampled = length(which(Shaken == TRUE)) / N_Trees #calculate percent of total trees which were shook for beating sheet
         ) %>% 
  group_by(Species, .add = T) %>% #add species grouping for the species count calculations
  mutate(TreeCounts = n()) %>% #counts of each tree species grouped by site and rep
  group_by(Site, Rep) %>% #revert to original grouping
  distinct(Species, TreeCounts, .keep_all = T) %>%  #remove duplicate rows
  select(!Shaken) %>% 
  ungroup() %>%  
  arrange(SiteRep)#remove groupings and arrange by SiteRep

#pivot to wide table with columns for each species with counts as value
Tree_Data_wide <- Tree_Data_long %>% 
  group_by(Site, Rep) %>% 
 pivot_wider(names_from = "Species", values_from = "TreeCounts",values_fn = mean, values_fill = 0) %>% #widen to calc diversity indices, filling NAs as 0
  ungroup() %>% 
  mutate(TreeShannon = vegan::diversity(select(.,"Acer negundo":"Magnolia acuminata")), #Shannon-Weiner Index
         TreeSimpson = vegan::diversity(select(.,"Acer negundo":"Magnolia acuminata"),"simpson"), #Simpson's Diversity Index
         TreeInvSimp = vegan::diversity(select(.,"Acer negundo":"Magnolia acuminata"),"invsimp"), #Simpson's Dominance Index
         TreeEvenness = TreeShannon/TreeRichness #Shannon Evenness 
  ) %>% #reorder the table for readability
  select(Site, Rep, Date,Time_Start, Time_End, Sample_Time,PropTrees_Sampled, TreeRichness, TreeShannon, TreeSimpson, TreeInvSimp, TreeEvenness, everything()) %>% 
  arrange(SiteRep)

#merge diversity scores back into long data
Tree_Data_long <- full_join(Tree_Data_long, select(Tree_Data_wide,SiteRep,  TreeShannon:TreeEvenness), by = "SiteRep")

#remove raw data
rm(Tree_Data_RAW)

#combi-table of plant and tree data 
#merge long tables
Veg_Data_long <- full_join(select(Plant_Data_long, !Site& !Rep& !Date), Tree_Data_long, by = join_by("SiteRep" == "SiteRep", "Species" == "Species")) %>%
  select(SiteRep,Species,Count, TreeCounts, everything()) %>% 
  group_by(SiteRep) %>%  #fill in NAs with values for that given Site and Rep
  fill(PlantRichness:TreeEvenness, .direction = "downup") %>% #replace NAs with 0s
  mutate_all(~replace(., is.na(.), 0)) %>% #add together counts for tree and cover plants
  mutate(TotalCounts = TreeCounts + Count,
         VegRichness =unique(n())) %>% 
  ungroup()

#pivot merged table
Veg_Data_wide <- Veg_Data_long %>% #remove separate counts
  select(!TreeCounts & !Count) %>% 
  pivot_wider(names_from = "Species", values_from = "TotalCounts",values_fn =sum, values_fill = 0) %>% 
  ungroup() %>% 
  mutate(VegShannon = vegan::diversity(select(.,"Carex pennsylvanica":"Vibrunum spp.")), #Shannon-Weiner Index
         VegSimpson = vegan::diversity(select(.,"Carex pennsylvanica":"Vibrunum spp."),"simpson"), #Simpson's Diversity Index
         VegInvSimp = vegan::diversity(select(.,"Carex pennsylvanica":"Vibrunum spp."),"invsimp"), #Simpson's Dominance Index
         VegEvenness = VegShannon/VegRichness) #Shannon Evenness 

#remove partial data
rm(Plant_Data_long, Plant_Data_wide, Tree_Data_long, Tree_Data_wide)

#build time interval table for AQ sorting
AQ_SiteSorting <- Veg_Data_long %>% 
  select(Site, Date, Rep, Time_Start, Time_End) %>% 
  mutate(Datetime_Start = paste(Date, Time_Start),
         Datetime_End = paste(Date, Time_End))

