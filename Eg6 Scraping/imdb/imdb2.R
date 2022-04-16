library(rvest) ## scraping tool
library(ggplot2) ## graph
library(ggthemes) ## make a cuter graph

## this gets: seasons, episodes and scores.
## link must look like the example at the end
## grouped by season
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
    ggtitle(paste("Score on IMDb of", name)) 
    # + annotate("text",x=Inf,y=-Inf,label="your name",
    #          hjust=1.1,vjust=-0.5,col="grey",cex=4,fontface = "bold", alpha = 0.8)
  if (line == TRUE){
    a <- a + geom_line()
  }
  if (trend == TRUE){
    a <- a + geom_smooth(alpha = 0.1,method = "loess", aes(color = Season), color = "gray") 
  }
  a
}

community <- getdf("https://www.imdb.com/title/tt1439629/episodes?season=", 6)
plotit("Community", community)
ggsave("community.png")

zatchbell <- getdf("https://www.imdb.com/title/tt0455295/episodes?season=",3)

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
plotit("Glee", glee, trend = TRUE)
ggsave("Glee.png")

mrrobot <- getdf("https://www.imdb.com/title/tt4158110/episodes?season=", 4)
plotit("Mr. Robot", mrrobot, trend = TRUE)
ggsave("Mr Robot.png")

test <- getdf("https://www.imdb.com/title/tt8879940/episodes?season=", 2)
plotit("Mythic Quest", test)
ggsave("Mythic Quest.jpg")

fma <- getdf("https://www.imdb.com/title/tt0421357/episodes?season=", 1)
fma$Season <- "Fullmetal Alchemist 2003"
fmab <- getdf("https://www.imdb.com/title/tt1355642/episodes?season=", 1)
fmab$Season <- "Fullmetal Alchemist Brotherhood"

fmas <- rbind(fma,fmab)
plotit("FMA", fmas)

galactic <- getdf("https://www.imdb.com/title/tt0096633/episodes?season=", 4)
plotit("Legend of the Galactic Heroes", galactic)

db <- getdf("https://www.imdb.com/title/tt0280249/episodes?season=", 9)
dbz <- getdf("https://www.imdb.com/title/tt0214341/episodes?season=", 16)
dbgt <- getdf("https://www.imdb.com/title/tt0139774/episodes?season=", 1)
dbsuper <- getdf("https://www.imdb.com/title/tt4644488/episodes?season=", 1)
db$Season <- "Dragon Ball"
dbz$Season <- "Dragon Ball Z"
dbgt$Season <- "Dragon Ball GT"
dbsuper$Season <- "Dragon Bal Supper"
dbseries <- rbind(db,dbz,dbgt,dbsuper)
dbseries$release <-  as.numeric(row.names(dbseries))
dbseries$Season <- factor(dbseries$Season, levels=unique(dbseries$Season)) 
plotit("Dragon Ball Series", dbseries, line = FALSE, trend = TRUE)

conan <- getdf("https://www.imdb.com/title/tt0131179/episodes?season=", 53)
conan$Season
plotit("Conan", conan, line = FALSE)
library(stringr)
as.numeric(substr(conan$Season, 8, 11))
conan$Season <- as.numeric(substr(conan$Season, 8, 11))
conan$Season
plotit("Conan", conan, line = FALSE)

b10_1 <- getdf("https://www.imdb.com/title/tt0760437/episodes?season=", 4)
b10_2 <- getdf("https://www.imdb.com/title/tt1622696/episodes?season=", 3)
b10_3 <- getdf("https://www.imdb.com/title/tt6148376/episodes?season=", 5)
b10_1$Season <- "Ben 10 (2005)"
b10_2$Season <- "Ben 10: Ultimate Alien (2010)"
b10_3$Season <- "Ben 10 Reboot (2016)"
b10_series <- rbind(b10_1, b10_2, b10_3)
b10_series$Season <- factor(b10_series$Season, levels=unique(b10_series$Season)) 
b10_series$release <- 1:nrow(b10_series)
plotit("Ben 10 Series", b10_series)

glee <- getdf