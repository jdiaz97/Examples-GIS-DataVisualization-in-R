library(rio)
library(ggplot2)
library(reshape2)

df <- import("1.xlsx")

## done to match papers style
cols <- c("SB" = "red", "IT" = "blue", "CO" = "green")
shapes <- c("SB" = 17, "IT" = 15, "CO" = 19)

## paper style version, but withour error bars cause data not available
for (j in unique(df$Evaluación)){
  for (i in unique(df$Especie)){
    a <- ggplot(df[df$Especie == i & df$Evaluación == j,], aes(x = Fecha, y = `Tamaño (cm)`, 
                                                               color = Tratamiento, 
                                                               group = Tratamiento, shape = Tratamiento)) + 
      guides(color = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
      geom_point() + 
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

## Tukey's test added info version. Most complete version.. Gotta fix multiplied letters.

for (j in unique(df$Evaluación)){
  for (i in unique(df$Especie)){
    a <- ggplot(df[df$Especie == i & df$Evaluación == j,], aes(x = Fecha, y = `Tamaño (cm)`, 
                                                               color = Tratamiento, 
                                                               group = Tratamiento, 
                                                               shape = Tukey)) + 
      guides(color = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
      geom_point(size = 7.5) + 
      geom_line() +
      theme_classic() +
      ylab(j) +
      ggtitle(i) +
      scale_color_manual(values = cols) +
      scale_shape_identity()
    print(a)
    ggsave(filename = paste(i, j, "Tukey1", ".png"))
  }
}

## Another solution for fixing Tukey's letters,
for (j in unique(df$Evaluación)){
  for (i in unique(df$Especie)){
    a <- ggplot(df[df$Especie == i & df$Evaluación == j,], aes(x = Fecha, y = `Tamaño (cm)`, 
                                                               color = Tratamiento, 
                                                               group = Tratamiento)) + 
      guides(color = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
      geom_point(size = 0) + 
      geom_text(
        aes(label = Tukey), size = 8) +
      geom_line() +
      theme_classic() +
      ylab(j) +
      ggtitle(i) +
      scale_color_manual(values = cols) +
      scale_shape_identity()
    print(a)
    ggsave(filename = paste(i, j, "Tukey2", ".png"))
  }
}