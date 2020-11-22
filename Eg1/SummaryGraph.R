library(tidyverse)
library(rstatix)
library(ggpubr)
library(openxlsx)
getwd()
## for next step be sure to setup your directory
df <- read.xlsx("Encuesta.xlsx")

df1 <-  df %>% 
  rename(
    "Glifosato (kg/ha)" = glifo,
  )

## Cool graph
ggsummarystats(
  df1, x = "Comuna", y = "Glifosato (kg/ha)", 
  ggpar.main = "Hi",
  summaries = c("n", "median", "min", "max", "sd","mean"),
  color = "Comuna", palette = "npg",
  ggfunc = ggboxplot, add = "point",
)
