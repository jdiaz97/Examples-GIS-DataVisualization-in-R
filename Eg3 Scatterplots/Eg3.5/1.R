library(rio) ## import
library(tidyr) ## gather
library(ggplot2) ## ggplot
library(grid)
library(extrafont)
loadfonts(device = "win")

df <- import("1.xlsx")
split <- data.frame("Nombre común" = df$`Nombre común`,
                    "Nombre cientifico" = df$`Nombre científico`,
                  do.call('rbind', strsplit(as.character(df$Mes),',',fixed=TRUE)), ## separate by commas
                  "Autor" = df$Autor)
## ignore or solve the error, code still works but not super efficient

finaldf <- split %>%  gather("var", "value", -Nombre.común, -Autor, -Nombre.cientifico)
finaldf$value <-  gsub(" ", "", finaldf$value, fixed = TRUE) ## eliminate spaces

finaldf$value <- factor(finaldf$value, levels = c("Diciembre", "Enero", "Febrero", "Marzo", "Abril",
                                                  "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre",
                                                  "Noviembre"))

finaldf$Nombre.completo <- paste(df$`Nombre científico`,"-",df$`Nombre común`)

ggplot(finaldf) + aes(x = value, y = Nombre.común, color = Nombre.común) +
  geom_point(size = 4.5) +
  xlab("") +
  ylab("") +
  scale_y_discrete(limits = rev) +
  theme_light() +
  theme(legend.position = "none",text=element_text(family="serif", size = 13, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  ggtitle("Fechas de recolección de semillas") +
  geom_text(aes(label = Autor, family = "serif", size = 12), x = 14.5) +
  coord_cartesian(xlim = c(0.9, 16), # This focuses the x-axis on the range of interest
                  clip = 'off')    # This keeps the labels from disappearing

ggsave("1.jpg", width = 13.9, height = 9)
