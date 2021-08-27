library(rvest) ## scraping tool
library(ggplot2) ## graph
library(ggthemes) ## make a cuter graph

## link must look like the example at the end

get_data <- function(html){
  df <- {}
  dfx <- {}
  site <- read_html(html)
  score <- (site %>% html_elements(".ipl-rating-star__rating") %>% html_text2())
  score <- score[score != "Rate"]
  score <- score[!score %in% c(as.character(0:10))]
  score <- as.numeric(score)
  dfx$score <- score
  as.data.frame(dfx)
}

getdf <- function(html,seasons){
  df <- {}
  for (i in 1:seasons){
    tempdf <- get_data(paste0(html,i))
    tempdf$Season <- paste("Season",i)
    df <- rbind(df,tempdf)
  }
  df$release <- as.numeric(row.names(df))
  df <- df[complete.cases(df),]
  df$Season <- factor(df$Season, levels = c(unique(df$Season)))
  df
}

plotit <- function(name,df, trend = FALSE, line = TRUE){
  a <- ggplot(df) + aes(x = release, y = score, color = Season) +
    geom_point() +
    scale_y_continuous(limits = c(min(df$score)-1,10), breaks = seq(0,10.0,1)) +
    theme_fivethirtyeight() +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(paste("Score on imdb of", name)) +
    annotate("text",x=Inf,y=-Inf,label="u/FinchoDM",
             hjust=1.1,vjust=-0.5,col="grey",cex=4,fontface = "bold", alpha = 0.8)
  if (line == TRUE){
    a <- a + geom_line()
  }
  if (trend == TRUE){
    a <- a + geom_smooth(alpha = 0.1,method = "loess", aes(color = score), color = "grey") 
  }
  a
}

community <- getdf("https://www.imdb.com/title/tt1439629/episodes?season=", 6)

plotit("Community", community)
ggsave("community.png")

superstore<- getdf("https://www.imdb.com/title/tt4477976/episodes?season=", 6)
plotit("Superstore", superstore)
ggsave("Superstore.png")

thesimpsons <- getdf("https://www.imdb.com/title/tt0096697/episodes?season=", 32)
plotit("The Simpsons", thesimpsons, line = FALSE)
ggsave("The Simpsons Again.png")

theoffice<- getdf("https://www.imdb.com/title/tt0386676/episodes?season=", 9)
plotit("The Office", theoffice)
ggsave("The Office.png")

glee<- getdf("https://www.imdb.com/title/tt1327801/episodes?season=", 6)
plotit("Glee", glee)
ggsave("Glee.png")

mrrobot <- getdf("https://www.imdb.com/title/tt4158110/episodes?season=", 4)
plotit("Mr. Robot", mrrobot, trend = TRUE)
ggsave("Mr Robot.png")

