library(rio) ## import()
library(ggplot2) ## ggplot()
library(lubridate) ## month()
library(RColorBrewer) ## colorRampPalette()
library(ghibli) ## ghibli_palettes$

df <- import("data.xlsx")
df <- df[complete.cases(df), ]
df$`Fecha Colecta` <- month(as.Date(df$`Fecha Colecta`, origin = "1899-12-30"))

dfstudy <- df[df$Origen == "Nativa", ]

ggplot(dfstudy) + 
  aes(y = `Nombre Común`, x=`Fecha Colecta`, colour = `Nombre Común`) +
  geom_point(size = 3.1) +
  theme_light() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = 1:14) +
  scale_y_discrete(limits=rev) +
  xlab("Mes de colecta") +
  ggtitle("Meses en los que CONAF ha recolectado semillas")

ggsave("grahp1.jpg", width = 4.6, height = 8.0)

## someone in the facebook group asked for this lol
# ggplot(dfstudy) + 
#   aes(y = `Nombre Común`, x=`Fecha Colecta`, color = Región, fill = Región) +
#   geom_point(size = 3.1) +
#   theme_light() +
#   theme(legend.position = "none") +
#   scale_x_continuous(breaks = 1:14) +
#   scale_y_discrete(limits=rev) +
#   xlab("Mes de colecta") +
#   facet_grid(cols = vars(Región)) +
#   scale_color_manual(values = mycolors)

## ghibli graph version
mycolors <- colorRampPalette(ghibli_palettes$PonyoMedium)(43)
ggplot(dfstudy) + 
  aes(y = `Nombre Común`, x=`Fecha Colecta`, colour = `Nombre Común`) +
  geom_point(size = 3.1, shape = 18) +
  scale_color_manual(values = mycolors, guide = guide_legend(legend.position = "none")) +
  theme_light() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = 1:14) +
  scale_y_discrete(limits=rev) +
  xlab("Mes de colecta") +
  ggtitle("Meses en los que CONAF ha recolectado semillas")
