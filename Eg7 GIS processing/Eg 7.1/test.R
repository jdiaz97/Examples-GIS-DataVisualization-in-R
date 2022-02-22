library(sf) ## several functions
library(rgeos) ## idk
library(tidyverse) ##d dplyr + ggplot
library(rgdal)
library(stringr) 

RM <- read_sf("Nativo RM.shp")

RM$FID_CBN_RE <- NULL 

RM$Especi1 <- paste0(RM$ESPECI1_CI," (",RM$ESPECI1_CO,")")
RM$Especi2 <- paste0(RM$ESPECI2_CI," (",RM$ESPECI2_CO,")")
RM$Especi3 <- paste0(RM$ESPECI3_CI," (",RM$ESPECI3_CO,")")
RM$Especi4 <- paste0(RM$ESPECI4_CI," (",RM$ESPECI4_CO,")")
RM$Especi5 <- paste0(RM$ESPECI5_CI," (",RM$ESPECI5_CO,")")
RM$Especi6 <- paste0(RM$ESPECI6_CI," (",RM$ESPECI6_CO,")")

RM$ESPECI1_CI <- NULL
RM$ESPECI2_CI <- NULL
RM$ESPECI3_CI <- NULL 
RM$ESPECI4_CI <- NULL
RM$ESPECI5_CI <- NULL
RM$ESPECI6_CI <- NULL
RM$ESPECI1_CO <- NULL
RM$ESPECI2_CO <- NULL
RM$ESPECI3_CO <- NULL 
RM$ESPECI4_CO <- NULL
RM$ESPECI5_CO <- NULL
RM$ESPECI6_CO <- NULL
RM$ESPCC1 <- NULL
RM$ESPCC2 <- NULL 

RM$Especi2[RM$Especi2 == "NA (NA)"] <- NA
RM$Especi3[RM$Especi3 == "NA (NA)"] <- NA
RM$Especi4[RM$Especi4 == "NA (NA)"] <- NA
RM$Especi5[RM$Especi5 == "NA (NA)"] <- NA
RM$Especi6[RM$Especi6 == "NA (NA)"] <- NA

species <- unique(c(RM$Especi1,RM$Especi2, RM$Especi3, RM$Especi4, RM$Especi4, RM$Especi5, RM$Especi6))
species <- species[!is.na(species)] ## remove missing values

get_good_name <- function(str){
  str <- gsub("[\\(\\)]", "", regmatches(str, gregexpr("\\(.*?\\)", str))[[1]]) ## extract value from parenthesis
  str <- str_replace_all(str, " ", "_") ## this would save us crushes, lol
}

createsf <- function(i){
  name <- get_good_name(species[i])
  print(paste("Proccessing the species:",name))
  assign(name, RM[RM$Especi1 %in% species[i] | RM$Especi2 %in% species[i] | RM$Especi3 %in% species[i] | RM$Especi4 %in% species[i] 
                  | RM$Especi4 %in% species[i] | RM$Especi5 %in% species[i] | RM$Especi6 %in% species[i],])
  
  assign(name, eval(parse(text = name))[1])
  print("Processing the shapefile")
  assign(name,st_make_valid(eval(parse(text=name))))
  # assign(name,st_combine(eval0(name)))
  # assign(name,st_as_sf(eval0(name)))
  # assign(name,st_make_valid(eval0(name))) 
  errors <- sum(!st_is_valid(eval(parse(text=name))))
  print(paste0("Number of errors: ",errors))
  if(errors == 0){
    write_sf(eval(parse(text=name)), paste0(name,".shp"))
    print("Creation successful")
  } else {
    print("Creation unsuccessful")
  }
}

for (i in 3:85){
  createsf(i)
}
species
