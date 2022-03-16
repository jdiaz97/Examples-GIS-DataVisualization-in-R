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
  if (str_ends(img_url,".jpg")){
    img_url
  } else {
    "-"
  }
}

create_df <- function(html){
  if (!is.error(html)){
    df <- data.frame(
      "nombre" = html %>% html_elements("h1") %>% html_text2(),
      "img_url" = get_img(html),
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
export(export_df, "data..csv")
export(export_df, "data..json")
export(export_df, "data..xlsx")
