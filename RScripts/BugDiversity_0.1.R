#2023-12-24: This code has been deprecated: trying to actually do a fn species analysis using separate tables

#THIS IS THE SCRIPT TO PROCESS BIODIVERSITY DATA OF COLLECTED INSECTS. THIS SCRIPT IS MEANT TO RUN <FOURTH> IN THE SCRIPT SEQUENCE
#RUNNING CODE OUT OF SEQUENCE WILL RESULT IN WORKFLOW FAILURE

#Read in data----
BugDiversity_RAW <- data.table::fread(file = "RData/input/BugDiversity.csv")

#repair broken site names (descriptions)----
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

#separate observations based on method of collection
#initialze empty variable
BugDiversity_RAW[, Method := ""]
#Netting (if has N at the end of description it is netting)
BugDiversity_RAW[str_detect(description, "N$"), Method := "Netting"]
#Beating Sheet (if it has BS at the end of description it is beating sheet)
BugDiversity_RAW[str_detect(description, "BS$"), Method := "Beating Sheet"]
#PROGRAMMER'S NOTE: seems like beating sheet has wayy less collections, I wonder if it had different insect assemblages (by order of course)

#classify observations based on site and replicate
#siterep
BugDiversity_RAW[, SiteRep := str_sub(str_remove(description, "[:space:]"), start = 1L, end = 3L)]
#Site
BugDiversity_RAW[, Site := str_remove(SiteRep, "[:digit:]")]
#Rep
BugDiversity_RAW[, Rep := str_extract(SiteRep, "[:digit:]")]

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
BugDiversity_RAW <- BugDiversity_RAW[FunctionalGroup != ""]


#Data clean-up----
#insert datetime from tree data
BugDiversity_RAW <- BugDiversity_RAW %>%  
  left_join(.,select(Veg_Data_wide, SiteRep, Date), by = "SiteRep")

#clean up raw data into functional group data
FN_Data_long <- BugDiversity_RAW %>% 
  filter(FunctionalGroup != "") %>% #filter out all bugs which could not be given a functional group
  select(Site, Rep, Date, Method, FunctionalGroup, SiteRep, taxon_order_name) %>% 
  group_by(SiteRep, Method, FunctionalGroup, taxon_order_name) %>% 
  summarise(FnGroupCount_Order = n()) %>% #calculate counts of each group per SiteRep and Method and Order
  group_by(SiteRep) %>% #regroup to calculate functional group richness by siterep 
  mutate(FnRichness = length(unique(FunctionalGroup))) %>% 
  group_by(.add = T, FunctionalGroup) %>% 
  mutate(FnOrderRichness = length(unique(taxon_order_name))) #richness of orders within a functional group at each site

#pivot table wider to calc diversity scores
FN_Data_wide <- FN_Data_long %>% 
  group_by(SiteRep, FunctionalGroup, Method) %>% 
  summarise(FnGroupCounts = sum(FnGroupCount_Order),
            FnRichness = unique(FnRichness)) %>% 
  pivot_wider(names_from = "FunctionalGroup", values_from = "FnGroupCounts",values_fill = 0) %>%  
  ungroup() 

#calculate diversity indices without method as a group
FNW_Div <- FN_Data_wide %>% 
  group_by(SiteRep) %>% 
  summarise(across("Fungivore":"Detritivore", .fns = sum), FnRichness = unique(FnRichness)) %>% 
  ungroup() %>% 
  mutate(FnShannon = vegan::diversity(select(.,"Fungivore":"Detritivore")), #Shannon-Weiner Index
         FnSimpson = vegan::diversity(select(.,"Fungivore":"Detritivore"), "simpson"), #Simpson Diversity Index
         FnInvSimp = vegan::diversity(select(.,"Fungivore":"Detritivore"), "invsimp"),
         FnEvenness = FnShannon/FnRichness)  %>% #Shannon evenness
        select(!"Fungivore":"Detritivore")


#calculate jaccard diversity scores between sites per method ----
#filter for just netting rows
FN_wide_N <-FN_Data_wide %>%
  filter(Method == "Netting")

#calc jaccard similarity indices and output to matrix
FN_jaccard_N <-as.matrix(vegan::vegdist(FN_wide_N[4:9], method = "jaccard"))
#edit col and row names for matrix
rownames(FN_jaccard_N) <- FN_wide_N$SiteRep
colnames(FN_jaccard_N) <- FN_wide_N$SiteRep


#filter for just beating sheet rows
FN_wide_BS <-FN_Data_wide %>%
  filter(Method == "Beating Sheet")

#calc jaccard similarity indices and output to matrix
FN_jaccard_BS <-as.matrix(vegan::vegdist(FN_wide_BS[4:9], method = "jaccard"))
#edit col and row names for matrix
rownames(FN_jaccard_BS) <- FN_wide_BS$SiteRep
colnames(FN_jaccard_BS) <- FN_wide_BS$SiteRep

#remove leftover objects
rm( FN_wide_BS, FN_wide_N)


