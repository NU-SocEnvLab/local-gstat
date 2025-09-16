#The next function will obtain a CBSA code for each census tract, remove any water bodies (empty geometry)
#and add a 5 digit county code (STATE + COUNTY) for county only estimates. 
#We will again use get_acs to retrieve CBSA geometry for the year of interest and then spatial join them to our smaller geometry 
#A table is defined but estimates are not needed so they are removed.
#The resulting dataframe will include estimates for each tract/block with tract/block geometry, CBSA code, COUNTY code,
#tract code, and summary variable

addGeo <- function(est, year){
  #calling in CBSA geometry 
  cbsa_i0 <- get_acs(
    geography   = "cbsa",
    year        = year,
    table       = "B03002",
    geometry    = TRUE,
    output      = "wide",
    cache_table = TRUE
  )
  
  cbsa <- subset(cbsa_i0, select = c(GEOID, NAME)) %>%
    rename(CBSA = GEOID, CBSA_NAME = NAME)
  
  #ADD CHECKS FOR CORRECT CRS
  
  #spatial join CBSA to Tracts
  est_sfjoin0 <- st_join(est, cbsa, join = st_within)
  
  #removing empty geometries 
  est_sfjoin <- est_sfjoin0 %>% filter(!st_is_empty(est_sfjoin0))
  
  #adding county code for tracts that fall outside a CBSA
  estFin <- est_sfjoin %>%
    mutate(county_cod = substring(GEOID, 1, 5), TRACTCE = substring(GEOID, 6, 11))%>%
    rename(JOIN_ID = GEOID, CBSAFP = CBSA)
  
  #subsetting to only needed variables
  estFin <- subset(estFin, select = -c(NAME, CBSA_NAME))
  
}


