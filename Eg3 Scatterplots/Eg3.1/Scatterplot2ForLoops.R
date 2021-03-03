library(rio)
library(ggplot2)

df <- import("1.xlsx")

## done to match papers style
cols <- c("SB" = "red", "IT" = "blue", "CO" = "green")
shapes <- c("SB" = 17, "IT" = 15, "CO" = 19)


for (j in unique(df$Evaluación)){
  for (i in unique(df$Especie)){
    a <- ggplot(df[df$Especie == i & df$Evaluación == j,], aes(x = Fecha, y = `Tamaño (cm)`, 
                                                               color = Tratamiento, 
                                                               group = Tratamiento, shape = Tratamiento)) + 
      guides(color = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
      geom_point(aes(shape = Tratamiento)) + 
      geom_line() +
      theme_classic() +
      ylab(j) +
      ggtitle(i) +
      scale_shape_manual(values = shapes) +
      scale_color_manual(values = cols)
    print(a)
    ggsave(filename = paste(i, j, ".png"))
  }
}

## Tukey info is missing, gotta find a way to incorporate it

  
