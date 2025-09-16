#--- function: gstat_area
# OBJECTIVE: to take census tracts/blaock groups with population > 0, calculate wieght matrix, identifiy data issues, 
# and loop through every CBSA/County available in the data to calculate the gstat and number of neighbors.
# INPUTS: data from ```gstat.r```
# OUTPUTS: dataframe of gstats and number of neighbors for each census tract/black group
#--- last updated: 8/31/2025
gstat_area <- function(dat, codeName, queen, variable_of_int, self_include = TRUE){
  # naming the variable codename whether it is cbsa or county
  names(dat)[names(dat) == codeName] <- "codeName"
  
  # unique list of CBSA and County codes
  CBSAs          <- unique(dat$codeName)
  
  # initialize list of CBSAs that have an issue
  CBSAs_NA       <- c() 
  
  # initializing dataset to store the gstat results
  results        <- data.frame(list(JOIN_ID = as.character(), 
                                    Gstat   = as.numeric(), 
                                    n_neigh = as.numeric()))
  
  # Divide tracts with 0 population from the rest
  cbsaG_zero_pop <- dat[dat$summary_est <= 0,]
  cbsaG          <- dat[dat$summary_est > 0,]
  
  # renaming the variable of interest to pop_of_int
  names(cbsaG)[names(cbsaG) == variable_of_int] <- 'pop_of_int'
  for (CBSA in CBSAs) {
    
    #We select only the tract within one cbsa, and retain only their ID, variable of interest, and geometry
    results_i <- cbsaG %>% 
      filter(codeName == CBSA) %>% 
      select(JOIN_ID, pop_of_int)
    
    #We create the weight matrices within each cbsa now
    #We check that there are more than one tract in the cbsa
    if ((nrow(results_i) > 1)) {
      q1 <- poly2nb(results_i, queen = queen)
      if (self_include){ q1 <- include.self(q1) }
      # We also check the tracts in the cbsa are connected.
      # This is the total number of links (shared borders) in the cbsa
      links <- sum( unlist(lapply(q1, function(x) x)))
      # The check, if there are any tracts at all that border
      if (links > 0){
        # row-stand. weights-info on what tracts border each tract and their weights
        q1      <- nb2listw(q1,style = "W", zero.policy = TRUE)
        #Weight matrix in the form of an actual matrix.
        W_m     <- listw2mat(q1)
        pos_col <- ncol(W_m)
        if (!(self_include)) { pos_col <- pos_col - 1 }
        results_i$n_neigh <- rowSums(W_m>0)
        # Case where there is no population of interest in the CBSA. Code = -199
        if (sum(results_i$pop_of_int) == 0){
          results_i$Gstat <- -199
        }
        else{
          # Calculate which (if any) tract is connected to all other tracts
          props <- results_i$n_neigh/pos_col
          # Usual lines to match the correct order
          names(props) <- rownames(W_m)
          props <- props[match(rownames(results_i), names(props))]
          W_m <- rowSums(W_m)
          if (any(W_m == 0)){
            CBSAs_NA[length(CBSAs_NA)+1] <- CBSA
          }
          #Calculate Getis-Ord "Hot Spots" for population of interest
          results_i$Gstat <- as.numeric(localG(results_i$pop_of_int,q1))
          # If a tract is connected to all other tracts, we have a division by 0
          # Signal this with code -299.
          results_i$Gstat[props == 1] = -299
        }
        # If there are not enough links
      } else {
        results_i$Gstat <- -399
        results_i$n_neigh <- 0 + self_include }
    }
    # Case where there is only one tract in the CBSA. Code = -99
    if (nrow(results_i) == 1){
      results_i$Gstat   <- -99
      results_i$n_neigh <- 0 + self_include }
    
    #Now store the data in a way that makes sense by attaching the new rows to the results dataframe
    results <- rbind(results, as.data.frame(results_i)[,c("JOIN_ID", "Gstat", 'n_neigh')])

  }
  
  # merge cbsaG and results
  names(cbsaG)[names(cbsaG) == 'pop_of_int'] <- variable_of_int
  cbsaG1 <- merge(cbsaG, results, all.x = TRUE)
  
  # Re-unite the 0 population tracts with the rest
  if (nrow(cbsaG_zero_pop !=0)){
    cbsaG_zero_pop$Gstat   <- -100
    cbsaG_zero_pop$n_neigh <- -100
    cbsaG_zero_pop <- cbsaG_zero_pop %>% relocate(geometry, .after = "n_neigh")
    
    cbsaFIN <- rbind(cbsaG1, cbsaG_zero_pop) #---#
  } else{
    cbsaFIN <- cbsaG1
  }
  
  # restoring the name of the area variable
  names(cbsaFIN)[names(cbsaFIN) == "codeName"] <- codeName
  
  return(cbsaFIN)
}



