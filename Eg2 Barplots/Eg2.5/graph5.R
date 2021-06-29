library(tidyverse)
library(foreign)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
## for next step be sure to setup your directory
# 
# R7 <- read.dbf(file = "Catastro_RV_R07_2016.dbf", as.is = TRUE)
# R8 <- read.dbf(file = "Catastro_RV_R08_2015.dbf", as.is = TRUE)
# R9 <- read.dbf(file = "Catastro_RV_R09_2014.dbf", as.is = TRUE)
# 
# ## fixing the data, mostly, and making changes so we can graph the dataframes.
# ## data not easily fixable because of bad naming of columns, there must be an easier way to do it.
# 
# R7 <-  R7 %>% 
#   rename(
#     "Provincia" = `NOM_PROV`,
#   )
# Encoding(R7$Provincia) <- "UTF-8"
# Encoding(R7$NOM_REG) <- "UTF-8"
# Encoding(R7$SUBUSO) <- "UTF-8"
# 
# R8 <-  R8 %>% 
#   rename(
#     "Provincia" = `NOM_PROV`,
#   )
# Encoding(R8$Provincia) <- "UTF-8"
# Encoding(R8$NOM_REG) <- "UTF-8"
# Encoding(R8$SUBUSO) <- "UTF-8"
# 
# R8$NOM_REG <- unique(R8$NOM_REG)[1] ## someone messed a tilde
# 
# R9 <-  R9 %>% 
#   rename(
#     "Provincia" = `NOM_PROV`,
#   )
# 
# 
# RÑ <- R8[R8$Provincia == "Ñuble", ]
# R8up <- R8[R8$Provincia != "Ñuble", ]
# 
# 
# R7f <- {}
# R8f <- {}
# R9f <- {}
# RÑf <- {}
# 
# R7f$SUBUSO <- R7$SUBUSO
# R7f$SUPERF_HA <- R7$SUPERF_HA
# R7f$Provincia <- R7$Provincia
# R7f$NOM_REG <- R7$NOM_REG
# 
# R8f$SUBUSO <- R8up$SUBUSO
# R8f$SUPERF_HA <- R8up$SUPERF_HA
# R8f$Provincia <- R8up$Provincia
# R8f$NOM_REG <- R8up$NOM_REG
# 
# R9f$SUBUSO <- R9$SUBUSO
# R9f$SUPERF_HA <- R9$SUPERF_HA
# R9f$Provincia <- R9$Provincia
# R9f$NOM_REG <- R9$NOM_REG
# 
# RÑf$SUBUSO <- RÑ$SUBUSO
# RÑf$SUPERF_HA <- RÑ$SUPERF_HA
# RÑf$Provincia <- RÑ$Provincia
# RÑf$NOM_REG <- "Región de Ñuble"
# 
# R7f <- as.data.frame(R7f) 
# R8f <- as.data.frame(R8f) 
# R9f <- as.data.frame(R9f) 
# RÑf <- as.data.frame(RÑf)
# 
# data <- rbind(R7f, R8f, R9f, RÑf)
# data$SUBUSO[data$SUBUSO == "Rotacion Cultivo-Pradera"] <- "Rotación Cultivo-Pradera" ## jesus 
# data$SUBUSO[data$SUBUSO == "Plantaciones"] <- "Plantación"
# data$SUBUSO[data$SUBUSO == "Terrenos de Uso Agricola"] <- "Terreno de Uso Agrícola"
# 
# dataf <- data[data$SUBUSO == "Plantación" | data$SUBUSO == "Bosque nativo" | 
#                 data$SUBUSO == "Bosque Nativo" | 
#                 data$SUBUSO == "Rotación Cultivo-Pradera" |
#                 data$SUBUSO == "Terreno de Uso Agrícola" ,]
# 
# export(dataf, "dataf.xlsx")
dataf <- import("dataf.xlsx")

colorscodes <- c("Bosque Nativo" = "limegreen",
                 "Plantación" = "lightgreen",
                 "Rotación Cultivo-Pradera" = "lightgreen",
                 "Terreno de Uso Agrícola" = "lightgreen")

plot1 <- ggplot(dataf, aes(y=SUBUSO, x=SUPERF_HA, fill = SUBUSO)) + 
  scale_fill_manual(values = colorscodes) +
  xlab("Área (hectáreas)") +
  ylab("Tipo de uso de suelo") +
  geom_bar(position="stack", stat="identity") +
  coord_flip() +
  facet_grid(. ~ NOM_REG ) +
  scale_y_discrete(guide = guide_axis(angle = 35)) +
  expand_limits(x = 0) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(expand = c(0, 0)) 

ggsave("Bosques nativos comparación.png", plot = plot1)
