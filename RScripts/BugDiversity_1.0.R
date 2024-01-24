#This the the most up to date version of the code which processes raw iNat data, and builds tow data tables
#table one is the diversity by order data table, ordered by site 

#RAW DATA PROCESSING----
#Read in Raw data from iNat
BugDiversity_RAW <- read_csv("RData/input/BugDiversity.csv")

#repair broken site names (descriptions)
BugDiversity_RAW <- data.table::as.data.table(BugDiversity_RAW)
#misnamed sites
BugDiversity_RAW[description == "A1 BS", description := "R1A BS"]
BugDiversity_RAW[description == "S2A", description := "S2A N"]
#empty site names
BugDiversity_RAW[url == "https://www.inaturalist.org/observations/166519747", description := "R1A BS"]
BugDiversity_RAW[url == "https://www.inaturalist.org/observations/167117357", description := "R1B BS"]
BugDiversity_RAW[url == "https://www.inaturalist.org/observations/172877967", description := "U1A N"]
BugDiversity_RAW[url == "https://www.inaturalist.org/observations/174752623", description := "U1C BS"]
BugDiversity_RAW[url == "https://www.inaturalist.org/observations/188271327", description := "U1C N"]
BugDiversity_RAW[url == "https://www.inaturalist.org/observations/190135230", description := "S2A N"]
BugDiversity_RAW[url == "https://www.inaturalist.org/observations/167117405", description := "R1B BS"]
#extraneous descriptions
BugDiversity_RAW[str_detect(description, "^bottom pair"), description := str_remove(description, "^bottom pair")]

#separate observations based on method of collection, and subjective site category
#initialze empty variables
BugDiversity_RAW[, Method := ""]
BugDiversity_RAW[ , Type := ""]

#Netting (if has N at the end of description it is netting)
BugDiversity_RAW[str_detect(description, "N$"), Method := "Netting"]
#Beating Sheet (if it has BS at the end of description it is beating sheet)
BugDiversity_RAW[str_detect(description, "BS$"), Method := "Beating Sheet"]

#Rural (if it starts with R)
BugDiversity_RAW[str_detect(str_squish(description), "^R"), Type := "Rural"]
BugDiversity_RAW[str_detect(str_squish(description), "^S"), Type := "Urban"]
BugDiversity_RAW[str_detect(str_squish(description), "^U"), Type := "Urban"]

#extract SiteRep
BugDiversity_RAW[,SiteRep := str_sub(str_squish(description), start = 1, end = 3)]
#extract site
BugDiversity_RAW[,Site:= str_remove(str_squish(SiteRep), "[:digit:]")]
BugDiversity_RAW[,Rep:= str_extract(str_squish(SiteRep), "[:digit:]")]

#Distribute Functional groups according to taxonomic rank, in order of highest ranks to lowest
#this ensures that the overriding FN is given to the lowest level of ranking (e.g. genus level ID overrides family level id, and so on)
#generate empty column 
BugDiversity_RAW[,FunctionalGroup := ""]

#order level----
#Omnivores
BugDiversity_RAW[taxon_order_name == "Opiliones" ,FunctionalGroup := "Omnivore"]

#Predators
BugDiversity_RAW[taxon_order_name == "Araneae" | #spiders
                   taxon_order_name == "Neuroptera" | #lacewings (larvae aka antlions)
                   taxon_order_name == "Pseudoscorpiones",
                 FunctionalGroup := "Predator"]

#Herbivores
BugDiversity_RAW[taxon_order_name == "Lepidoptera" | 
                   taxon_order_name == "Psocodea" |
                   taxon_order_name == "Orthoptera"
                 ,FunctionalGroup := "Herbivore"]

#Detritivores
BugDiversity_RAW[taxon_order_name == "Entomobryomorpha" | 
                   taxon_order_name == "Isopoda" ,
                 FunctionalGroup := "Detritivore"]
#parasites
BugDiversity_RAW[taxon_order_name == "Ixodida", #ticks
                 FunctionalGroup := "Parasitoid"]


#suborder level----
#Herbivores
BugDiversity_RAW[taxon_suborder_name == "Auchenorrhyncha"| 
                   taxon_suborder_name == "Sternorrhyncha" | #non-heteroptera hemipterans
                   taxon_suborder_name == "Symphyta", #sawflies
                 FunctionalGroup := "Herbivore"]


#superfamily level----
#Predators
BugDiversity_RAW[taxon_superfamily_name == "Empidoidea" | #Diptera 
                   taxon_superfamily_name == "Anystoidea", #Mites
                 FunctionalGroup := "Predator"] 

#family level----
#Omnivores
BugDiversity_RAW[taxon_family_name == "Elateridae" | 
                   taxon_family_name == "Cantharidae" |
                   taxon_family_name == "Tenebrionidae" |
                   taxon_family_name == "Staphylinidae" | #beetle families
                   taxon_family_name == "Formicidae" #ants
                 ,FunctionalGroup := "Omnivore"]
#Predators
BugDiversity_RAW[taxon_family_name == "Reduviidae" | #Hemiptera
                   taxon_family_name == "Chaoboridae" |
                   taxon_family_name == "Asilidae",#Diptera
                 FunctionalGroup := "Predator"]
#Herbivores
BugDiversity_RAW[taxon_family_name == "Scarabaeidae" |
                   taxon_family_name == "Byturidae" |
                   taxon_family_name == "Cerambycidae" |
                   taxon_family_name == "Curculionidae" |
                   taxon_family_name == "Chrysomelidae" |
                   taxon_family_name == "Mordellidae" |
                   taxon_family_name == "Scatopsidae" | 
                   taxon_family_name == "Attelabidae" | #beetles
                   taxon_family_name == "Pentatomidae" | 
                   taxon_family_name == "Acanthosomatidae" | 
                   taxon_family_name == "Pachygronthidae" |
                   taxon_family_name == "Miridae" ,  #hemiptera
                 FunctionalGroup := "Herbivore"]
#Detritivores
BugDiversity_RAW[taxon_family_name =="Limoniidae" |
                   taxon_family_name =="Sepsidae" |
                   taxon_family_name =="Lauxaniidae" |
                   taxon_family_name == "Scatopsidae" | #Diptera
                   taxon_family_name == "Melandryidae" |
                   taxon_family_name == "Scirtidae" |
                   taxon_family_name == "Ptinidae", #beetles
                 FunctionalGroup := "Detritivore"]
#Fungivores
BugDiversity_RAW[taxon_family_name == "Mycetophilidae" | #Diptera
                   taxon_family_name == "Cryptophagidae" | 
                   taxon_family_name == "Latridiidae", #beetles
                 FunctionalGroup := "Fungivore"]
#Parasites/oids
BugDiversity_RAW[taxon_family_name == "Braconidae" | 
                   taxon_family_name == "Ichneumonidae" | 
                   taxon_family_name == "Eurytomidae" |
                   taxon_family_name == "Pteromalidae"|
                   taxon_family_name == "Eulophidae"|
                   taxon_family_name == "Platygastridae" |
                   taxon_family_name == "Figitidae" |
                   taxon_family_name == "Diapriidae" | #parasitoid wasps
                   taxon_family_name == "Culicidae", #mosquitoes
                 FunctionalGroup := "Parasitoid"]
#subfamily level----
#Omnivores
BugDiversity_RAW[taxon_subfamily_name == "Malachiinae" ,FunctionalGroup := "Omnivore"] #beetle subfamily
#Herbivores
BugDiversity_RAW[taxon_subfamily_name == "Apioninae" | 
                   taxon_subfamily_name == "Anaspidinae" ,#beetles
                 FunctionalGroup := "Herbivore"]

#genus level----
#Predators
BugDiversity_RAW[taxon_genus_name == "Chilocorus" | 
                   taxon_genus_name == "Coccidula"
                 ,FunctionalGroup := "Predator"]
#Herbivores
BugDiversity_RAW[taxon_genus_name == "Chlorops", FunctionalGroup := "Herbivore"]

#Detritivores
BugDiversity_RAW[taxon_genus_name == "Gyrophaena", #beetles
                 FunctionalGroup := "Detritivore"]
#fungivores
BugDiversity_RAW[taxon_genus_name == "Psyllobora",
                 FunctionalGroup == "Fungivore"]

#filter out taxa not assigned a functional group
BugDiversity_Filtered <- BugDiversity_RAW[FunctionalGroup != ""]

#order by SiteRep and reset uid from iNat's db, deselect extraneous columns
BugDiversity_Filtered <- BugDiversity_Filtered %>% 
  as_tibble() %>% 
  arrange(SiteRep) %>% 
  rename(Order = taxon_order_name) %>% 
  select(!starts_with("taxon")) %>% 
  mutate(id = 1:nrow(BugDiversity_Filtered),
         FnG_Order = paste(FunctionalGroup, Order, sep = "-")) %>% #combine order and functional group as new category
  dplyr::select(!longitude & !latitude & !positional_accuracy 
                & !description & !observed_on_string & !quality_grade & 
                  !tag_list  & !scientific_name & !species_guess) 

#merge true DateTime, location, and sampling effort into from Vegetation Data
BugDiv_Decent <- BugDiversity_Filtered %>% 
  left_join(select(Veg_Data_wide, SiteRep, ends_with("Dec"), Sample_Time, Date), by = "SiteRep") %>% 
  
  select(id, SiteRep,Type, Method,Date, Sample_Time, ends_with("Dec"), FnG_Order, Order, FunctionalGroup, everything())

#calculate the richness of functional groups, orders, and unique combos per siterep
BugDiv_Richness <- BugDiv_Decent %>% 
  group_by(SiteRep) %>% 
  mutate(Order_Richness = length(unique(Order)), # num ofunique orders in group
         FnG_Richness = length(unique(FunctionalGroup)),#num of unique functional groups
         Total_Richness =length(unique(FnG_Order))) %>% 
  group_by(.add = T, FunctionalGroup) %>% 
  mutate(Order_in_FnG = length(unique(Order)), 
         In_vs_Total = Order_in_FnG/Order_Richness) %>% #number of orders within a funtional group 
  ungroup() 

#temp data tables to merge into indices table
BugDiv_InvsOut <- BugDiv_Richness %>% 
  group_by(SiteRep, FunctionalGroup) %>% 
  summarise(In_vs_Total = unique(In_vs_Total))

BugDiv_Rich <- BugDiv_Richness %>% 
  group_by(SiteRep) %>% 
  summarise(Order_Richness = unique(Order_Richness),
            FnG_Richness = unique(FnG_Richness),
            Total_Richness = unique(Total_Richness))

#calculate relative counts of functional group and order combos
BugDiv_Abundance <- BugDiv_Decent %>% 
  group_by(SiteRep, Method, FnG_Order) %>% 
  summarise(FnG_Order_n = n(),
            across(Date:LongitudeDec, unique)) %>% 
  pivot_wider(names_from = "FnG_Order", values_from = "FnG_Order_n", values_fill = 0) %>% 
  ungroup()

#calculate diversity indices
BugDiv_Indices <- BugDiv_Abundance %>% 
  group_by(SiteRep) %>%
  summarise(across("Fungivore-Coleoptera":"Predator-Neuroptera", sum)) %>% 
  mutate(FnGO_Shannon = hillR::hill_taxa(select(.,"Fungivore-Coleoptera":"Predator-Neuroptera"), q =1), #Shannon-Weiner Index
         FnGO_InvSimp = hillR::hill_taxa(select(.,"Fungivore-Coleoptera":"Predator-Neuroptera"), q = 2), #Inverse Simpson Diversity Index
         ) %>% 
  ungroup() %>% 
  left_join(BugDiv_Rich, by = "SiteRep") 
  


 