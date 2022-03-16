library(rio)
library(stringr)

df <- import("data.xlsx", encoding = "UTF-8")

df <- df[df$img_url != "-",]
df <- df[df$habito == "Árbol",]
df <- df[df$origen == "Nativa" | df$origen == "Endémica",]
df <- df[,1:3,]
df$nombre <- word(df$nombre,1,2, sep=" ")

export(df, "cleaned_data.json")