library(tidyverse)
library(foreign)
## Choose the directory manually with choose.dir()  
## or using RStudio Session -> set working directory -< To source file location
## How many DBF files you want to read?
thelist <- 1:6
## read them!
for (i in thelist){
  assign(paste0('Sitio',i), read.dbf(paste0("Buffer sitio ",i,".dbf")))
}
## sugar code used on later functions
todf <- function(string,numb){
  eval(parse(text = paste0(string,numb)))
}

makegraph <- function(i){
  a <- ggplot(data = todf("Sitio",i), aes(y=area.ha, x=USO_ACTUAL)) +
    xlab("Tipo de Uso de suelo") +
    ylab("?rea (hect?reas)") +
    geom_bar(position="stack", stat="identity") +
    ggtitle(paste0("Usos suelo en sitio ",i)) +
    coord_flip() +
    scale_y_continuous(expand = c(0, 0))
  print(a)
  ggsave(filename = (paste0("Usos de suelo en sitio ", i, ".png")), height = 6, width = 9, unit = "in")
}
makegraph(1)
## hardcoded to take a quick look at the data
Sitio1 %>%
  group_by(USO_ACTUAL) %>%
  summarise(count = sum(area.ha)) 

## print all the cool graphs
for (i in thelist){
  makegraph(i)
}
