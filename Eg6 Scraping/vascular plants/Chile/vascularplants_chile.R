library(rvest)
library(magrittr)
library(rio)

## DON'T RUN

site <- function(id){
  try(assign("thehtml",read_html(paste0("SORRY I CAN'T GIVE IT",id))
         %>% html_elements("section"), envir = globalenv()), silent = TRUE)
}

getdata <- function(n){
  (thehtml %>% 
     html_elements(".field-items") %>% html_text2())[n]
}

adddata <- function(){
  df <- {}
  df$nombre <- thehtml %>% html_elements("h1") %>% html_text2()
  df$familia <- getdata(1)
  df$sinonimos <- getdata(2)
  df$hábito <- getdata(3)
  df$origen <- getdata(4)
  df$distribuicion <- getdata(5)
  df$rango <- getdata(6)
  df$paiseslimitrofes <- getdata(7)
  df$nombrecomún <- getdata(8)
  df$notas <- getdata(9)
  df <- as.data.frame(df)
  assign("df1", rbind(df1,df), envir = globalenv())
}
## create dummy df
site(4354)
df1 <- {}
df1$nombre <- thehtml %>% html_elements("h1") %>% html_text2()
df1$familia <- getdata(1)
df1$sinonimos <- getdata(2)
df1$hábito <- getdata(3)
df1$origen <- getdata(4)
df1$distribuicion <- getdata(5)
df1$rango <- getdata(6)
df1$paiseslimitrofes <- getdata(7)
df1$nombrecomún <- getdata(8)
df1$notas <- getdata(9)
df1 <- as.data.frame(df1)

## make if statement to check if it exists and then apply getdata() for i in  20:9999
rm(thehtml)

for(i in 20:4353){
  print(i)
  site(i)
  if (exists("thehtml")){
    adddata()
  }
  try(rm(thehtml), silent = TRUE)
}
for(i in 4355:9999){
  print(i)
  site(i)
  if (exists("thehtml")){
    adddata()
  }
  try(rm(thehtml), silent = TRUE)
}

export(dfx, "data.xlsx")
