library(rio)
library(ggplot2)

data <- import("Lluvia en Chillán.xlsx")

ggplot(data) + aes(x = Año, y = Precipitación) +
  geom_bar(position="stack", stat="identity", fill = "lightblue") +
  geom_smooth(method = "loess") +
  ylab("Precipitación (mm)") +
  scale_y_continuous(limits = c(0,1600)) +
  theme_bw()

ggsave("Precipitación en Chillán.png")

