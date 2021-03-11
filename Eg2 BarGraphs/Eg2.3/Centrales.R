library(rio)
library(ggplot2)
## set working directory to source file location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
## first graph

DF1 <- import("graph1.xlsx")
DF1$resultado__potencia <- as.numeric(DF1$resultado__potencia)

hist(DF1$resultado__potencia[DF1$resultado__tipo == "HIDRAULICA PASADA"], 
     xlab = "Potencia [MW]", 
     ylab = "Frecuencia", 
     col = "lightblue",
     main = "Distribuición de la frecuencia de la potencia de centrales hidroeléctricas de pasada", 
     breaks = 50
     )
axis(side=1, at=seq(0,100, 10), labels=seq(0,100,10))

## second graph

# M <- as.data.frame(table(DF1$resultado__region[DF1$resultado__tipo == "HIDRAULICA PASADA"]))
# M
# sum(M$Freq) ## must be 109
# export(M, "region.xlsx")
## PROPER() in excel, and order regions by geography
M <- import("graph2.xlsx")
M$Var1 <- factor(M$Var1, levels = M$Var1) ## makes sure to display in the same order as the df

ggplot(data = M, aes(x=Var1, y=Freq)) +
  geom_bar(position="stack", stat="identity", show.legend = FALSE, fill = "darkturquoise") + 
  ylab("Cantidad de centrales hidroeléctricas") +
  xlab("Región") + 
  ggtitle("Distribuición de centrales de pasada por Región") +
  coord_flip() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 16), breaks = seq(0, 16, by = 1)) +
  theme_classic()

## Third graph
# DF3 <- as.data.frame(table(DF1$resultado__comuna[DF1$resultado__tipo == "HIDRAULICA PASADA"]))
# DF3
# sum(DF3$Freq) ## must be 109
# export(DF3, "comunas.xlsx")
## PROPER in excel and  added regions
DF3 <- import("graph3.xlsx")
DF3$Var1 <- factor(DF3$Var1, levels = DF3$Var1) ## makes sure to display in the same order as the df
ggplot(data = DF3, aes(x=Var1, y=Freq, fill = Región)) +
  geom_bar(position="stack", stat="identity") + 
  ylab("Cantidad de centrales hidroeléctricas") +
  xlab("Comuna") + 
  ggtitle("Distribuición de centrales de pasada por Comuna") +
  coord_flip() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8), breaks = seq(0, 8, by = 1)) +
  theme_classic()


