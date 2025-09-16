#--- function: gstat
# OBJECTIVE: run gstat_calc for census tracts within a given region
# INPUTS: geoType (string)
#         endYear (integer)
#         raceGroups (string)
#         states (string): default all 50 states and DC
#         queen (boolean): default TRUE
# OUTPUTS:res (dataframe)
#--- last updated: 8/31/2025

gstat <- function(geoType, endYear, raceGroups, states = stateList, queen = TRUE){
  # obtaining proportions for each race group for all census tracts in the states listed
  est0     <- stateLoop(geo = geoType, year = endYear, stateList = states)
  
  # adding geographic info to the tracts
  estFIN   <- addGeo(est = est0, year = endYear)
  
 
  # initializing a list of results for each race group
  allRaces        <- vector("list", length = length(raceGroups) + 1)
  names(allRaces) <- c("Ref", raceGroups)
  allRaces$Ref    <- estFIN
  
  # for each race...
  for(race in raceGroups){
    # calculate the gstat
    fin  <- gstat_calc(df = estFIN, queen = queen, var = race)
    
    #rename variables
    fin1 <- fin %>% 
      #rename(GEOID = JOIN_ID) %>% 
      rename_with(function(x) paste(str_sub(race, end = -3), x, sep = "_"), c("Gstat", "n_neigh"))
    
    # save the results in allRaces
    allRaces[[race]] <- fin1
  }
  
  # merge all race results together
  res <- allRaces %>% reduce(left_join, by = "JOIN_ID")
  
  #sub-setting to only needed columns and removing geometry
  res<- res[, -c(2:16)] %>%
    
    st_drop_geometry(res)
  
  #final rename
  res <- res %>% rename(FIPS_ID = JOIN_ID)
  
  return(res)
}


