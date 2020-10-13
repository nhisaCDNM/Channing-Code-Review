############################################################################################################################
# Code to link NDVI 270m focal statistics (SEASONAL) for Landsat NDVI to NHS, NHS II, HPFS
# Channing Code Review; October 13, 2020
# Written by: Hari Iyer

# Inputs: NHS, NHS II, HPFS geocoded addresses with lat and long for each year
#         NDVI 1230m focal statistics monthly rasters (Directory: G:\Built_Environment\BE_Data\Geographic_Data\GEE_Landsat_NDVI)
# Outputs: wide file with one row per ID, monthly NDVI for every year of follow-up, per cohort

# Features: User specifies years that need to be pulled in vector (see code below). Wrote three functions to apply temporal range, 
# extract, and export table with cohort IDs and monthly data.
############################################################################################################################

############################################
## Step 1: Read in Cohort Tabular Data
############################################

library(tidyverse)
library(haven)
library(sf)
library(raster)
library(spData)
library(readr)

## List of 35 regions created in Google Earth Engine to be used to filter cohort members and filter rasters
## Naming conventions identical across cohort and NDVI dataset
GEEregions <- c("Colorado", "Florida", "Illinois", "IndianaOhio", "Michigan", "Minnesota", "MississippiAlabama", "Nevada", "NewMexico", "Oklahoma", "Utah", "Wisconsin", "Oklahoma", "NewMexico", "Arizona", "Colorado", "Utah", "Nevada", "Wyoming", "Idaho", "Florida", "SouthCarolinaGeorgia", "SouthAtlantic1", "Kansas", "MissouriIowa", "Nebraska", "Minnesota", "NorthSouthDakota", "Montana1","NC1","NC2","TX1","TX2","TX3","CA1","CA2", "Montana2")

## Path to Landsat 8 directory (either temporary drive on UNIX or exteral hard drive)
newPath <- 'E:/NDVI (final)/FOCAL STATISTICS/1230m/Landsat 8/'

## Create vector of NDVI raster years captured by Landsat 8 satellite
ls8_yy<-c("2013", "2014", "2015", "2016", "2017", "2018") 

###################################################### Functions below ######################################################
## Define extract to avoid conflicts from dplyr/tidyr
extract <- raster::extract   

## Function to apply temporal filter
temporal_filter<-function(inlist,region,yyyy){ ## make sure the year is a string
  strings.to.find = c(region,yyyy)
  str <- paste0("(?=.*", strings.to.find,")", collapse="") ## this code will process the strings and allow "AND" logical construction 
  sublist <- grepl(str, inlist, perl=TRUE) ## this should keep the right files
  return(inlist[sublist])
}

## function to capture images within a user-defined temporal range - for cohorts, we want to also pull for subsequent two years
## "eco" for economy size
extract_eco_ls5<-function(inlist, indat, GEEregion, yyyy){ ## make sure the year is a string
  ## need to get images for yearin, along with subsequent two years
  
  year_n <- as.numeric(yyyy)
  year_1 <- as.character(year_n + 1)
  year_2 <- as.character(year_n + 2)
  
  ## get list of images corresponding to all three years
  
  minilist_y1 <-temporal_filter(inlist,GEEregion,yyyy)
  minilist_y2 <-temporal_filter(inlist,GEEregion,year_1)
  minilist_y3 <-temporal_filter(inlist,GEEregion,year_2)
  
  minilist<-c(minilist_y1, minilist_y2 ,minilist_y3) # combine the lists of rasters for three years
  
  ## extract
  
  bStack <- stack(minilist)
  cohort_year<-indat[indat$yearin==year_n,]  ## will only keep addresses for this yearin
  cohort_small<-cohort_year[cohort_year$GEE_region==GEEregion,] ## will keep addresses in year in region
  out <- extract(bStack, cohort_small, df=TRUE)
  out$unid <- paste(cohort_small$unid) ## need to replace with my ID variable - "key" for cohorts
  
  return(out)
}

## Function loops over user-defined vector of years, applies the extract for yearin + 2, creates a dataframe and exports to .csv
exp_ndvi_ls<-function(yearvec, satlist, indat, filename, GEEregion){
  output_ls <- vector(mode = "list", length = length(yearvec))
  for(i in seq_along(yearvec)){
    output_ls[[i]]<-extract_eco_ls5(satlist, indat, GEEregion, yearvec[i])
  }
  ls_cohort<-dplyr::bind_rows(output_ls)
  rm(output_ls, i)
  file_path <- str_c("Y:/Built_Environment/BE_Data/Geographic_Data/GEE_Landsat_NDVI/Exports/1230m/",filename) 
##  write.csv(ls_cohort, file=file_path, na = ".") ## change
return(ls_cohort)
}

## Create a loop for states (by census sub-region)
output_state <- vector(mode = "list", length = length(GEEregions))
for(j in seq_along(GEEregions)){
  
  cohort <- read_sas("Y:/Built_Environment/BE_Data/Geographic_Data/GEE_Landsat_NDVI/all_wstate.sas7bdat")
  
  ## Create new column for GEE_region
  cohort$GEE_region <- ""
  
  ## Filter cohort dataset, row = census subregion, column = GEE_region and assign GEE region name
  cohort[cohort$SUB_REGION=="New England",c("GEE_region")] <- "NewEngland"
  cohort[cohort$SUB_REGION=="Middle Atlantic",c("GEE_region")] <- "MiddleAtlantic"
  
  ## Filter cohort dataset, row = state fips, column = GEE_region and assign GEE region name
  cohort[cohort$STATE_ABBR=="WI",c("GEE_region")] <- "Wisconsin"
  cohort[cohort$STATE_ABBR=="MI",c("GEE_region")] <- "Michigan"
  cohort[cohort$STATE_ABBR=="IL",c("GEE_region")] <- "Illinois"
  cohort[cohort$STATE_ABBR=="MN",c("GEE_region")] <- "Minnesota"
  cohort[cohort$STATE_ABBR=="NE",c("GEE_region")] <- "Nebraska"
  cohort[cohort$STATE_ABBR==c("IN", "OH"),c("GEE_region")] <- "IndianaOhio"
  cohort[cohort$STATE_ABBR==c("KY", "TN"),c("GEE_region")] <- "KentuckyTennessee"
  cohort[cohort$STATE_ABBR==c("MS", "AL"),c("GEE_region")] <- "MississippiAlabama"
  cohort[cohort$STATE_ABBR==c("MO", "IA"),c("GEE_region")] <- "MissouriIowa"
  cohort[cohort$STATE_ABBR==c("ND", "SD"),c("GEE_region")] <- "NorthSouthDakota"
  cohort[cohort$STATE_ABBR==c("KS"),c("GEE_region")] <- "Kansas"
  cohort[cohort$STATE_ABBR==c("FL"),c("GEE_region")] <- "Florida"
  cohort[cohort$STATE_ABBR==c("WA", "OR"),c("GEE_region")] <- "WashingtonOregon"
  cohort[cohort$STATE_ABBR==c("ID"),c("GEE_region")] <- "Idaho"
  cohort[cohort$STATE_ABBR==c("WY"),c("GEE_region")] <- "Wyoming"
  cohort[cohort$STATE_ABBR==c("CO"),c("GEE_region")] <- "Colorado"
  cohort[cohort$STATE_ABBR==c("AZ"),c("GEE_region")] <- "Arizona"
  cohort[cohort$STATE_ABBR==c("NM"),c("GEE_region")] <- "NewMexico"
  cohort[cohort$STATE_ABBR==c("NV"),c("GEE_region")] <- "Nevada"
  cohort[cohort$STATE_ABBR==c("UT"),c("GEE_region")] <- "Utah"
  
  ###############################################
  ## Step 2: Convert Cohort Tables to Shapefiles
  ###############################################
  
  coordinates(cohort)=~nlong+nlat ## adding coordinates
  
  proj4string(cohort)=CRS("+init=epsg:4326") # set it to lat-long
  cohort = spTransform(cohort,CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))
  

  ## Make sure newPath points to folder containing rasters (temporary directory nhair0a)
  list_ls8 <- list.files(path=newPath, full.names=TRUE, pattern = ".tif$")
  ## 
  
  ########################################################################################################################################
  output_state[[j]]<- exp_ndvi_ls(ls8_yy, list_ls8, cohort, str_c(GEEregions[j],"_ls8_1230m.csv"), GEEregions[j]) ## exports region CSVs
  ## ls8_s<-read.csv("/pc/nhair0a/Built_Environment/BE_Data/Geographic_Data/GEE_Landsat_NDVI/1230m (seasonal)/ls8_seasonal_cohorts.csv")
  ## summary(ls8_s)
  ## rm(ls8_s)
  
}

## Combine state-by-state output
ls8_expNoDupes <-dplyr::bind_rows(output_state)
file_path <- "Y:/Built_Environment/BE_Data/Geographic_Data/GEE_Landsat_NDVI/Exports/ls8_1230_13reg.csv" ##change to temp folder, mannually change this by landsat number
write.csv(ls8_expNoDupes, file=file_path) ## change
