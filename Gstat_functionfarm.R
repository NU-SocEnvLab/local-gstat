#============================================================#
#                  Gstatistics function farm                 #
#      By: Cyanna McGowan cyanna.mcgowan@northwestern.edu    #
#                  Last modified: 8/31/2025                  #
#                                                            #
# Below are a series of function to use while creating the   #
# Getis-Ord Gi statistic fo any year and population group    #
#============================================================#


#All of census variables needed for the gstat will be in ACS table B03002: Total Population. 
#ACS year corresponds to the LAST year of the 5-year estimate.
#This function will create a vector of populations of interest from table B03002
#The result will be a vector of values that represent the table names of each population group
race_vars <- function(white, black, native, asian, HIPI, hispanic, HispWhite, 
                      HispBlack, HispNative, HispAsian, HispHIPI){
  race_vars <- c(
    White = white,
    Black = black,
    Native = native,
    Asian = asian,
    HIPI = HIPI,
    Hispanic = hispanic,
    HispWhite = HispWhite,
    HispBlack = HispBlack,
    HispNative = HispNative,
    HispAsian = HispAsian,
    HispHIPI = HispHIPI
  )
}

#creating vector of variables of interest
racevar <- race_vars(white= "B03002_003", black= "B03002_004", native="B03002_005", asian="B03002_006",
                     HIPI="B03002_007", hispanic="B03002_012", HispWhite= "B03002_013", 
                     HispBlack= "B03002_014", HispNative= "B03002_015", HispAsian= "B03002_016",
                     HispHIPI= "B03002_017")

#We will use tidycensus get_acs function to source all counts of each variable of interest and total population at the geographic level
#Below we are creating a vector of values that represent all 50 states and DC and excludes US territories. We then load all other needed functions.
stateList  <- unique(force(fips_codes)$state)[1:51]

#--- stateLoop
source("GstatFunctions/stateLoop.R")

#--- addGeo
source("GstatFunctions/addGeo.R")

#--- gstat_area
source("GstatFunctions/gstat_area.R")

#--- gstat_calc
source("GstatFunctions/gstat_calc.R")

#--- gstat
source("GstatFunctions/gstat.R")





