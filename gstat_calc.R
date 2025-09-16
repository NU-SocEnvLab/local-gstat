
#--- function: gstat_calc
# OBJECTIVE: calculate the g-statistic for census tracts within a given region
# INPUTS: geoType (string)
#         endYear (integer)
#         raceGroups (string)
#         states (string): default all 50 states and DC
#         queen (boolean): default TRUE
# OUTPUTS:res (dataframe)
#--- last updated: 12/16/2024
gstat_calc <- function(df, queen, var, self_include = TRUE){
  
  #subsetting to determine how many tracts fall within county, cbsa, or both
  cbsaG   <- df %>% dplyr::filter(!is.na(CBSAFP))#only CBSA needed
  countyG <- df %>% dplyr::filter(is.na(CBSAFP)) #only counties needed
  
    
  # first, calculate gstat for all CBSAs
  cbsaFIN <- gstat_area(dat             = cbsaG, 
                        codeName        = "CBSAFP", 
                        queen           = queen, 
                        variable_of_int = var)
  
  # then, calculate gstat for all counties
  countyFIN <- gstat_area(dat             = countyG, 
                          codeName        = "county_cod", 
                          queen           = queen, 
                          variable_of_int = var)

  # recombining cbsa and county results
  gstatFIN <- rbind(cbsaFIN, countyFIN) 
  gstatFIN <- subset(gstatFIN, select=c(JOIN_ID, Gstat, n_neigh))
  gstatFIN <- st_drop_geometry(gstatFIN)
  
  return(gstatFIN)
  
}


