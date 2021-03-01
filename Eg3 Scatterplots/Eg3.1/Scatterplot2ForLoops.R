library(rio)
library(ggplot2)

df <- import("1.xlsx")

for (j in unique(df$Evaluación)){
  for (i in unique(df$Especie)){
    a <- ggplot(df[df$Especie == i & df$Evaluación == j,], aes(x = Fecha, y = `Tamaño (cm)`, 
                                          color = Tratamiento, group = Tratamiento)) + 
      guides(color = guide_legend(reverse = TRUE)) +
      geom_point() + 
      geom_line() +
      theme_classic() +
      ylab(j) +
      ggtitle(i)
    print(a)
    ggsave(filename = paste(i, j, ".png"))
  }
}
