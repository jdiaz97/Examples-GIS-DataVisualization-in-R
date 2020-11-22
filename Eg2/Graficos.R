library(tidyverse)
library(foreign)
library(viridis)
## Choose the directory manually or using RStudio Session -> set working directory -< To source file location
## choose.dir()
## How many DBF files you want to read?
thelist <- 1:6
## read them!
for (i in thelist){
  assign(paste0('Sitio',i), read.dbf(paste0("Buffer sitio ",i,".dbf")))
}
## sugar code
todf <- function(string,numb){
  eval(parse(text = paste0(string,numb)))
}

makegraph <- function(i){
  a <- ggplot(data = todf("Sitio",i), aes(y=area.ha, x=USO_ACTUAL)) +
    xlab("Tipo de Uso de suelo") +
    ylab("Área (hectáreas)") +
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T) +
    ggtitle(paste0("Usos suelo en sitio ",i)) +
    theme(text = element_text(size=11),
          axis.text.x = element_text(angle=90, hjust=1)) 
  xlab("")
  return(a)
}
## hardcoded to take a quick look at the data
Sitio1 %>%
  group_by(USO_ACTUAL) %>%
  summarise(count = sum(area.ha)) 

## print all the cool graphs
for (i in thelist){
  print(makegraph(i))
}
