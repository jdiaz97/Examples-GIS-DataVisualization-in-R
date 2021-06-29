library(tidyverse)
library(rio)
library(lubridate)

data <- import("data.xlsx")

data$hora <- hour(data$Fecha) 
test <- data %>%
  group_by(hora) %>%
  summarise(mean = mean(`Mensajes por segundo`), n = n())

test <- as.data.frame(test)

ggplot(test) + aes(y = mean, x = hora, color = mean) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_point() +
  geom_line() +
  xlab("Hora") + ylab("Mensajes por segundo")

minute(data$Fecha)
