#Next, this function will loop through each state in statelist and compile all 
#census vaiables at geographic leavel of interest. It will then create proportion estimates for each 
#population in the variable list. If there is zero population in the tract, proportion estimate will be 
#set to 0 and handled later. Finally it will retain only the populations of interest.

stateLoop <-function(geo, year, stateList= stateList, vars= racevar, sum_var="B03002_001"){
  allStates0 <- vector(mode = "list", length = length(stateList))
  names(allStates0) <- stateList
  for (state in stateList) {
    est_i0 <- get_acs(
      geography   = geo, 
      state       = state,
      variables   = vars,
      summary_var = sum_var,
      year        = year, 
      geometry    = TRUE,
      output      = "wide",
    )%>%
      mutate(
        nhBlack_p    = if_else(summary_est == 0, 0, (BlackE / summary_est)),
        nhWhite_p    = if_else(summary_est == 0, 0, (WhiteE / summary_est)),
        nhNative_p   = if_else(summary_est == 0, 0, (NativeE / summary_est)),
        nhAsian_p    = if_else(summary_est == 0, 0, (AsianE / summary_est)),
        nhHIPI_p     = if_else(summary_est == 0, 0, (HIPIE / summary_est)),
        Hispanic_p   = if_else(summary_est == 0, 0, (HispanicE / summary_est)),
        HisWhite_p   = if_else(summary_est == 0, 0, (HispWhiteE / summary_est)),
        HispBlack_p  = if_else(summary_est == 0, 0, (HispBlackE / summary_est)),
        HispNative_p = if_else(summary_est == 0, 0, (HispNativeE / summary_est)),
        HispAsian_p  = if_else(summary_est == 0, 0, (HispAsianE / summary_est)),
        HispHIPI_p   = if_else(summary_est == 0, 0, (HispHIPIE / summary_est))
      )
    allStates0[[state]] <- est_i0
  }
  # merge into one dataframe
  est30 <- Reduce(rbind, allStates0)
  est30 <- subset(est30, select= c(GEOID, NAME, nhBlack_p, nhWhite_p, nhNative_p, nhAsian_p,
                                   nhHIPI_p, Hispanic_p, HisWhite_p, HispBlack_p, HispNative_p,
                                   HispAsian_p, HispHIPI_p, summary_est))
  return(est30)
}
