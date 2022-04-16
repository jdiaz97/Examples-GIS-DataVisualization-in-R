library(rvest) ## read_html() and stuff
library(rio) ## export()
library(stringr) ## str_ends()

is.error <- function(x) inherits(x, "try-error")

get_html<- function(id){
  try(read_html(paste0("http://catalogoplantas.udec.cl/?q=node/",id)), silent = TRUE)
}

get_data <- function(site,n){
  (site %>% 
     html_elements(".field-items") %>% html_text2())[n]
}

get_img <- function(site){
  img_url <- (site %>% html_elements("a") %>% html_attr("href"))[7]
  if (str_ends(img_url,".jpg") | str_ends(img_url,".JPG")){
    img_url
  } else {
    "-"
  }
}

get_tiny_img <- function(site){
  tinyimg_url <- (site %>% html_elements("img") %>% html_attr("src"))[2]
  if (grepl("itok",tinyimg_url) & (grepl(".jpg",tinyimg_url) | grepl(".JPG",tinyimg_url))){
    tinyimg_url
  } else {
    "-"
  }
}

get_map_url <- function(site){
  map_url <- (site %>% html_elements(".group-right") %>%  html_elements("a") %>% html_attr("href"))
  if(!identical(map_url,character(0))){
    if (str_ends(map_url,".png") | str_ends(map_url,".PNG")){
      map_url
    } else {
      "-"
    }
  } else {
    "-"
  }
}


create_df <- function(html){
  if (!is.error(html)){
    df <- data.frame(
      "nombre" = (html %>% html_elements("h1") %>% html_text2()),
      "large_img_url" = get_img(html),
      "tiny_img_url" = get_tiny_img(html),
      "map_url" = get_map_url(html),
      "familia" = get_data(html,1),
      "sinonimos" = get_data(html,2),
      "habito" = get_data(html,3),
      "origen" = get_data(html,4),
      "distribuicion"  = get_data(html,5),
      "rango" = get_data(html,6),
      "paiseslimitrofes" = get_data(html,7),
      "nombre_comun" = get_data(html,8),
      "notas" = get_data(html,9)
    )
    df
  }
}

final_df <- data.frame()
for (i in 1:6500){
  df <- create_df(get_html(i))
  final_df <- rbind(final_df,df)
  if (is.null(df)){
    print(paste(i,"Failure"))
  } else {
    print(paste(i,"Success"))
  }
}


## get good data 
export_df <- final_df[complete.cases(final_df),]
export(export_df, "data.csv")
export(export_df, "data.json")
export(export_df, "data.xlsx")


