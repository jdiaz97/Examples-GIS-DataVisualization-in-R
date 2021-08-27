library(rvest) ## scraping tool
library(ggplot2) ## graph
library(ggthemes) ## make a cuter graph
library(ggimage)

getdata <- function(html,n){
  df <- {}
  vect <- seq(3,7*n,7) ## hardcoded implementation of a vector that will use to get the score
  site <- read_html(html)
  
  name <- site %>% html_elements(".disco_release") %>% html_elements(".disco_info") %>% html_text2()
  name <- name[1:n]
  
  ratings <- site %>% html_elements(".disco_release") %>% html_elements(".disco_ratings") %>% html_text2()
  ratings <- ratings[1:n]
  
  
  score <- as.numeric((site %>% html_elements(".disco_release") %>% html_children())[vect] %>% html_text2())
  
  a <- (site %>% html_nodes(xpath = '//*/img') %>% html_attr('src'))[1:n+1]
  b <- (site %>% html_nodes(xpath = '//*/img') %>% html_attr('data-src'))[1:n+1]
  a <- as.data.frame(a[complete.cases(a)])
  b <- as.data.frame(b[complete.cases(b)])
  names(a) <- "img"
  names(b) <- "img"
  a
  b
  img <- as.data.frame(rbind(a,b))
  df$img <- img
  df$ratings <- ratings
  df$score <- score
  df$name <- name
  df <- as.data.frame(df)
  df$release <- as.numeric(row.names(df))
  df$img <- paste0("https:", df$img)
  
  # for (i in 1:nrow(df)){
  #   download.file(df$img[i], paste(df$release[i],".png"), mode = "wb")
  # }
  # df$filename <- paste(df$release,".png")
  df
}

plotit <- function(df,name){
  ggplot(df) + aes(x = release, y = score) +
    geom_image(aes(image = img)) +
    theme_minimal() +
    scale_y_continuous(limits = c(0,5), expand = c(0,0)) +
    scale_x_continuous(breaks = 1:50) +
    xlab("Order of release") +
    ylab("Score") +
    ggtitle(paste0("Score of ",name,"'s single albums on Rate Your Music")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ## gotta fix img 3 by hand because rate your music has a wrong album cover 
  
  ggsave(paste(name,".jpg"), width = 5.6, height = 5.6)
}

df <- getdata(
  html = "https://rateyourmusic.com/artist/charly-garcia",
  n = 16 
)

## for this particular case we have to fix the img 3 by hand, 
## rate your music failed to get a good Piano Bar album cover

plotit(df, "Charly Garcia")

df <- getdata(
  html = "https://rateyourmusic.com/artist/luis-alberto-spinetta",
  n = 18
)

plotit(df, "Spinetta")
