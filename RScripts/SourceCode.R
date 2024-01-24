## this is the source code for table manupulations based on taxonomic rank ascertained 
#Helper Functions:----

#Percentage of observations at a specified taxonomic rank
#ARGS:
#dt: dataframe/table which to use
#depth: taxonomic rank which to stop percent calculations at, as a  character string (eg order, family, genus, etc.). 
#filter_rank: if the query should be filtered for a specific taxonomic ranking (e.g. "order", "family:, etc.). defaults to NA, where no filter will be applied
#filter_name: if filter rank is given, what is the name of the rank to filter by? (e.g. only look at obs which are hymenoptera = filter_rank "order", filter_name = "Hymenoptera")
#OUTPUT:
#function outputs a list of each taxon rank to the given depth, with each element being a rank. each element holds a vector of length 3, contaning character outputs
Percent_At_Taxon <- function(dt, depth = NA, filter_rank = NA, filter_name = NA){
  #coerce dt to data.table
  dt <- data.table::as.data.table(dt)
  #vector of taxonomic groups from available columns
  taxon_groups <- str_subset(colnames(dt), "(?<=taxon_)[:lower:]+(?=_name)")
  #remove NAs
  taxon_groups <- taxon_groups[!is.na(taxon_groups)]
  #clip taxon groups based on depth
  taxon_groups<- taxon_groups[1:ifelse(is.na(depth) == T, -1,str_which(taxon_groups,paste("_", depth, "_", sep = "")))]
  
  #is the query filtered?
  if(is.na(filter_rank) == TRUE){
    #calculate total number of observations from total number of rows
    TotalObs <- nrow(dt)
    #loop through taxon groups to calculate percent for each
    unfiltered_percent_list <- list()
    for(t in 1:length(taxon_groups)){
      #current rank query string
      current_query_string <- taxon_groups[t]
      #calculate total number of observations of given taxon group t
      TotalQueriedObs <-nrow(dt[dt[[current_query_string]] != ""])
      #calculate percent of observations at given rank
      PercentAtRank <- TotalQueriedObs/TotalObs*100
      #output results to list as vector
      unfiltered_percent_list[[t]] <- c(paste("Total Observations at given rank:", TotalQueriedObs),
                                        paste("Percent of Observations at given rank:", round(PercentAtRank, 2), "%"),
                                        paste("Total Observations in dataframe:", TotalObs))
    }
    #rename list elements
    names(unfiltered_percent_list) <- str_extract(taxon_groups, "(?<=taxon_)[:lower:]+(?=_name)")
    return(unfiltered_percent_list)
    
  } else { #if filtering is not NA 
    #remove superfluous taxon names according to given rank
    taxon_groups <- taxon_groups[str_which(taxon_groups, paste("_", filter_rank, "_", sep = "")):length(taxon_groups)]
    #query string for filtering
    filter_query_string <- paste("taxon",filter_rank,"name", sep = "_")
    #calculate total number of observations in the filtered set
    TotalObs <- nrow(dt[dt[[filter_query_string]] == filter_name])
    #loop through taxon groups to calculate percent for each
    filtered_percent_list <- list()
    for(t in 1:length(taxon_groups)){
      #current rank query string
      current_query_string <- taxon_groups[t]
      #calculate total number of observations of queried rank by filter
      TotalQueriedObs <- nrow(dt[dt[[current_query_string]] != "" & dt[[filter_query_string]] == filter_name])
      #calculate percent of observations at given rank
      PercentAtRank <- TotalQueriedObs/TotalObs*100
      #output results to list as vector
      filtered_percent_list[[t]] <- c(paste("Total Observations at given rank:", TotalQueriedObs),
                                      paste("Percent of Observations at given rank:", round(PercentAtRank, 2), "%"),
                                      paste("Total Observations in dataframe:", TotalObs))
    }
    #rename list elements
    names(filtered_percent_list) <- str_extract(taxon_groups, "(?<=taxon_)[:lower:]+(?=_name)")
    return(filtered_percent_list)
  }
  
}

#Filter the dataframe to individuals which meet taxon ranking criteria
#ARGS:
#dt: dataframe/table which to use
#depth: taxonomic rank which to stop percent calculations at, as a  character string (eg order, family, genus, etc.). 
#group_rank: if the query should be grouped for a specific taxonomic ranking (e.g. "order", "family:, etc.). defaults to order, and cannot be NA
#threshold: numeric value from 0 - 100, indicating at what percent rank to accept (e.g., if threshold = .7 and coleoptera obsrvations have reached 75% depth to family, they will have a table made in the return list)
#OUTPUT:
#data frame containing all observations
tBox_Filter <- function(dt, depth, threshold = 70, group_rank = "order"){
  #coerce dt to data.table
  dt <- data.table::as.data.table(dt)
  #create output list
  return_list <- list()
  #iterate through given group rank of dt, passing args to Percent_At_Taxon function
  for(t in 1:length(unique(dt[[paste("taxon_",group_rank,"_name", sep ="")]]))){
    #current group string
    current_taxobox <- unique(dt[[paste("taxon_",group_rank,"_name", sep ="")]])[t]
    #build list element
    #call Percent_at_taxon
    PAT_call <- Percent_At_Taxon(dt = dt, depth = depth, filter_rank = group_rank, filter_name = current_taxobox)
    
    #extract return values at the desired depth
    percent_t <- as.numeric(str_extract(PAT_call[depth][[1]][2], "[:digit:]+") )
    
    #check if desired threshold is passed
    if(is.na(percent_t) != T & percent_t > threshold){
      #add only elements which are at the given depth
      return_list[[t]] <- dt[dt[[paste("taxon_",group_rank,"_name", sep ="")]] == current_taxobox & dt[[paste("taxon_",depth,"_name", sep ="")]] != ""] %>% 
        as_tibble() %>%
        select(!longitude & !latitude & !positional_accuracy 
               & !description & !observed_on_string & !taxon_id & !id & !quality_grade & 
                 !tag_list  & !scientific_name & !species_guess) %>% 
        left_join(select(Veg_Data_wide, SiteRep, ends_with("Dec"), Sample_Time, Date), by = "SiteRep")  %>% 
        select( SiteRep,Method,Type ,Date, Sample_Time, ends_with("Dec"), starts_with("taxon"), FunctionalGroup, everything()) 
      #coerce to df
      return_list[[t]] <- as.tibble(return_list[[t]])
    } else {
      return_list[[t]]<- NA
    }
    names(return_list)[t] <- current_taxobox 
  }
  #remove null values
  return_list <- return_list[!is.na(return_list)]
  #bind rows together
  return_df <- bind_rows(return_list, .id = "id") %>%
    mutate(id = rank(id, ties.method = "min")) %>% 
    group_by(taxon_order_name) 
  return(return_df)
}

Full <- tBox_Filter(BugDiversity_RAW, depth = "family", threshold = 66, group_rank = "order")

#calc diversity indices for each of the elements of the output list
diversity_list <- list()
for(d in 1:length(Full)){
  diversity_list[[d]] <- Full[[d]] %>% 
    group_by(SiteRep, taxon_family_name, Method) %>% 
    summarise(across(c(Type:LongitudeDec, FunctionalGroup), unique), family_n = n()) %>% 
    pivot_wider(names_from = taxon_family_name, values_from = family_n, values_fill = 0) %>% 
    ungroup() %>% 
    mutate(Richness = hillR::hill_taxa(select(., ends_with("dae")), q = 0),
           Shannon = hillR::hill_taxa(select(., ends_with("dae")), q = 1),
           Simpson = hillR::hill_taxa(select(., ends_with("dae")), q = 2))
    
  #assign name based on taxon order
  names(diversity_list)[[d]] <- names(Full)[[d]]
}

summary_list <-list()
for(d in 1:length(diversity_list)){
  summary_list[[d]] <- diversity_list[[d]] %>% 
    group_by(FunctionalGroup) %>% 
    mutate(across(Richness:Simpson, .fns = list(mean= mean, se = ~sd(.x, na.rm = T)/sqrt(n())))) %>% 
    summarise(across(c(ends_with(c("mean", "se")), SiteRep), unique))
  
  names(summary_list)[[d]] <- names(diversity_list)[[d]]
}

#plotting diversity of families across siterep and site type 
ggplot(data = diversity_list$Coleoptera) +
  geom_point(aes(x = SiteRep, y = Simpson, fill = Method, group = Type), shape = 21, size = 2.5) +
  geom_point(data = summary_list$Coleoptera, aes(x = SiteRep, y = Simpson_mean)) +
  geom_errorbar(data = summary_list$Coleoptera, aes(x = SiteRep, y =Simpson_mean, ymin = Simpson_mean -2*Simpson_se,ymax = Simpson_mean +2*Simpson_se)) +
  facet_wrap(~Type, scales = "free_x")

