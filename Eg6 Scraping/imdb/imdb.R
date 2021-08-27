library(rvest)
library(ggplot2)

## links provided MUST be simple/compact mode and release by date


getdata <- function(html){
  ## gotta be ordered by release date
  site <- read_html(html)
  
  name <- site %>% html_elements(".lister-item-header") %>% html_text2()
  year <- substr(substr(name, nchar(name) - 4, nchar(name)), 1,4 )
  score <- as.numeric(site %>% html_elements(".col-imdb-rating") %>% html_text2())
  
  ## this will make the name cuter, but it's kinda hardcoded and incomplete, could be cuter.
  name <- substr(name, 1, nchar(name)-7)
  
  ## create the df that returns the function when called
  df <- {}
  df$name <- name
  df$year <- year
  df$score <- score
  df <- as.data.frame(df)
  df
}

create_html_vector <- function(html){
  htmlfinal <- html
  if ("Next »"  %in% (read_html(html) %>% html_elements("a") %>% html_text2())){
  i <- 1
  while ("Next »" %in% (read_html(tail(htmlfinal, n=1)) %>% html_elements("a") %>% html_text2())){
  n <- 1 + 50*i 
  toadd <- paste0(html,"&start=",n)
  htmlfinal <- append(htmlfinal, toadd)
  i <- i+1
  }
  }
  htmlfinal
}

createdf <- function(html){
  vectors <- create_html_vector(html)
  df <- {}
  for(i in vectors){
    df <- rbind(df,getdata(i))
  }
  df <- as.data.frame(df)
  df$release <- as.numeric(rownames(df))
  df <- df[complete.cases(df),]
  df <- df[df$score != 0.0,]
  df
}

## generic purposes plot
plotit <- function(name,df,line = FALSE){
  a <- ggplot(df) + aes(x = release, y = score, color = year) +
    geom_point() +
    geom_line() +
    scale_y_continuous(limits = c(0,10), breaks = seq(0,10.0,1)) +
    theme_gray() +
    xlab("Episode number") +
    ylab("Score") +
    ggtitle(paste("Score on imdb of", name))
  if (line == TRUE){
    a <- a + geom_smooth(method = "loess") 
  }
  a
}

df1 <- createdf("https://www.imdb.com/search/title/?series=tt3322312&view=simple&sort=release_date,asc")
df2 <- createdf("https://www.imdb.com/search/title/?series=tt3322310&view=simple&sort=release_date,asc")
df3 <- createdf("https://www.imdb.com/search/title/?series=tt2357547&view=simple&sort=release_date,asc")
df4 <- createdf("https://www.imdb.com/search/title/?series=tt3322314&sort=release_date,asc&view=simple")
df5 <- createdf("https://www.imdb.com/search/title/?series=tt4230076&sort=release_date,asc&view=simple")
df1$series <- "Daredevil"
df2$series <- "Iron Fist"
df3$series <- "Jessica Jones"
df4$series <- "Luke Cage"
df5$series <- "The Defenders"

dfx <- rbind(df1,df2,df3,df4,df4)

ggplot(dfx) + aes(x = release, y = score, color = series) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(5,10), breaks = seq(0,10.0,1)) +
  theme_gray() +
  xlab("Episode number") +
  ylab("Score") +
  ggtitle(paste("Score on imdb of Marvel Series"))

ggsave("Marvel Series.png")
