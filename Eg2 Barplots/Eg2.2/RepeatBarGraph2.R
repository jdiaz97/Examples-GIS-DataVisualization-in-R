library(tidyverse)
library(foreign)
getwd()
## for next step be sure to setup your directory

R1 <- read.dbf(file = "Catastro_RV_R01_2016.dbf", as.is = TRUE)
R2 <- read.dbf(file = "Catastro_RV_R02_1997.dbf", as.is = TRUE)
R3 <- read.dbf(file = "Catastro_RV_R03_1997.dbf", as.is = TRUE)
R4 <- read.dbf(file = "Catastro_RV_R04_2014.dbf", as.is = TRUE)
R5 <- read.dbf(file = "Catastro_RV_R05_2013.dbf", as.is = TRUE)
R6 <- read.dbf(file = "Catastro_RV_R06_2013.dbf", as.is = TRUE)
R7 <- read.dbf(file = "Catastro_RV_R07_2016.dbf", as.is = TRUE)
R8 <- read.dbf(file = "Catastro_RV_R08_2015.dbf", as.is = TRUE)
R9 <- read.dbf(file = "Catastro_RV_R09_2014.dbf", as.is = TRUE)
R11 <- read.dbf(file = "Catastro_RV_R11_2011.dbf", as.is = TRUE)
R12 <- read.dbf(file = "Catastro_RV_R12_2005.dbf", as.is = TRUE)
R13 <- read.dbf(file = "Catastro_RV_R13_2013.dbf", as.is = TRUE)
R15 <- read.dbf(file = "Catastro_RV_R15_2015.dbf", as.is = TRUE)
alldfs <- ls()
## fixing the data, mostly, and making changes so we can graph the dataframes.
## data not easily fixable because of bad naming of columns, there must be an easier way to do it.
R1 <-  R1 %>% 
  rename(
    "Provincia" = `NOM_PROV`,
  )
Encoding(R1$Provincia) <- "UTF-8"
Encoding(R1$NOM_REG) <- "UTF-8"
Encoding(R1$SUBUSO) <- "UTF-8"

R2 <-  R2 %>% 
  rename(
    "Provincia" = `PROVINCIA`,
    "SUBUSO" =  `NOMBRE`,
    "NOM_REG" =  `REGION`,
  )
R2$NOM_REG <- "Región de Antofagasta"
R3 <-  R3 %>% 
  rename(
    "Provincia" = `PROVINCIA`,
    "SUBUSO" =  `NOMBRE`,
    "NOM_REG" =  `REGION`,
  )
R3$NOM_REG <- "Región de Atacama"
R4 <-  R4 %>% 
  rename(
    "Provincia" = `NOM_PROV`,
  )
Encoding(R4$Provincia) <- "UTF-8"
Encoding(R4$NOM_REG) <- "UTF-8"
Encoding(R4$SUBUSO) <- "UTF-8"

R5 <-  R5 %>% 
  rename(
    "Provincia" = `NOM_PRO10`,
  )
R5$NOM_REG <- "Región de Valparaiso"
R6 <-  R6 %>% 
  rename(
    "Provincia" = `NOM_PRO10`,
  )

R7 <-  R7 %>% 
  rename(
    "Provincia" = `NOM_PROV`,
  )
Encoding(R7$Provincia) <- "UTF-8"
Encoding(R7$NOM_REG) <- "UTF-8"
Encoding(R7$SUBUSO) <- "UTF-8"

R8 <-  R8 %>% 
  rename(
    "Provincia" = `NOM_PROV`,
  )
Encoding(R8$Provincia) <- "UTF-8"
Encoding(R8$NOM_REG) <- "UTF-8"
Encoding(R8$SUBUSO) <- "UTF-8"

R9 <-  R9 %>% 
  rename(
    "Provincia" = `NOM_PROV`,
  )
R11 <-  R11 %>% 
  rename(
    "Provincia" = `PROVINCIA`,
    "SUBUSO" = `USO_TIERRA`,
  )
R11$NOM_REG <- "Región de Aysén"
Encoding(R11$Provincia) <- "UTF-8"
Encoding(R11$SUBUSO) <- "UTF-8"
R11$Provincia[R11$Provincia == "Aisén"] <- "Aysén"

R12 <-  R12 %>% 
  rename(
    "Provincia" = `PROVINCIA`,
    "NOM_REG" = `REGION`,
    "SUBUSO" = `NOMBRE`,
  )
R12$NOM_REG <- "Región de Magallanes"

R13 <-  R13 %>% 
  rename(
    "Provincia" = `NOM_PRO10`,
  )
Encoding(R13$Provincia) <- "UTF-8"
Encoding(R13$NOM_REG) <- "UTF-8"
Encoding(R13$SUBUSO) <- "UTF-8"

R15 <- R15 %>%
  rename(
    "Provincia" = `NOM_PROV`,
  )
Encoding(R15$Provincia) <- "UTF-8"
Encoding(R15$NOM_REG) <- "UTF-8"
Encoding(R15$SUBUSO) <- "UTF-8"

## let's graph
makegraph <- function(REGION){
  B <- unique(REGION$NOM_REG)[1]
  a <- ggplot(REGION, aes(y=SUBUSO, x=SUPERF_HA, fill = Provincia)) +
     xlab("Área (hectáreas)") +
     ylab("Tipo de uso de suelo") +
     geom_bar(position="stack", stat="identity") +
     ggtitle(B) +
     expand_limits(x = 0) +
     scale_x_continuous(expand = c(0, 0))
  print(a)
  ggsave(filename = (paste0("Usos de suelo en ", B, ".png")), height = 6, width = 9, unit = "in")
}

## REPEATED GRAPHS WITH DIFFERENT DATAFRAMES
for (i in alldfs){
  makegraph(eval(parse(text = i)))
}
## R11 is broken tho