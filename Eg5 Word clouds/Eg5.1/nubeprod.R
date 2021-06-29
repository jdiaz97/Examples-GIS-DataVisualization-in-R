library(tm)
library(wordcloud)
library(pdftools)
library(tidyr)
library(ggwordcloud)

corpus <- pdf_text("ProgramaJadue.pdf") %>% strsplit(split = "\n")
corpus <- Corpus(VectorSource(corpus))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
m <- as.matrix(TermDocumentMatrix(corpus))
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq=v)
# wordcloud(d$word, d$freq, random.order= FALSE, rot.per=0.3,
#           scale=c(4,.5), max.words=101, colors= brewer.pal(8,"Dark2"))

## another option
plot0 <- ggplot(d[1:42,], aes(label = word,
                     size=freq,   
                     color = factor(sample.int(10, nrow(d[1:42,]), replace =T)))) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 10) +
  ggtitle("Palabras más usadas en el programa de Jadue") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = -2))
ggsave(filename = "Programa de Jadue.png", plot = plot0)
hist0 <- ggplot(data = d[1:25,], aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(position="stack", stat="identity", show.legend = FALSE, fill = "darkturquoise") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  ggtitle("Palabras más usadas en el programa de Jadue") +
  ylab("Frecuencia") +
  xlab("Palabras") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) 
ggsave(filename = "Hist de Jadue.png", plot = hist0)


## BORIC
corpus <- pdf_text("ProgramaBoric.pdf") %>% strsplit(split = "\n")
corpus <- Corpus(VectorSource(corpus))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
corpus <- tm_map(corpus, removeWords, c("manifiesto", "proceso", "primarias", "programático",
                                        "ejes", "programáticos")) ## repeat every page
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
m <- as.matrix(TermDocumentMatrix(corpus))
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq=v)
# wordcloud(d$word, d$freq, random.order= FALSE, rot.per=0.3,
#           scale=c(4,.5), max.words=101, colors= brewer.pal(8,"Dark2"))
plot1 <- ggplot(d[1:42,], aes(label = word,
                     size=freq,   
                     color = factor(sample.int(10, nrow(d[1:42,]), replace =T)))) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 10) +
  ggtitle("Palabras más usadas en el programa de Boric") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = -2))
ggsave(filename = "Programa de Boric.png", plot = plot1)
hist1 <- ggplot(data = d[1:25,], aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(position="stack", stat="identity", show.legend = FALSE, fill = "darkturquoise") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  ggtitle("Palabras más usadas en el programa de Boric") +
  ylab("Frecuencia") +
  xlab("Palabras") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) 
ggsave(filename = "Hist de Boric.png", plot = hist1)

## part 2
corpus <- pdf_text("ProgramaBriones.pdf") %>% strsplit(split = "\n")
corpus <- Corpus(VectorSource(corpus))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
corpus <- tm_map(corpus, removeWords, c("ello")) ## repeat every page
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
m <- as.matrix(TermDocumentMatrix(corpus))
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq=v)
# wordcloud(d$word, d$freq, random.order= FALSE, rot.per=0.3,
#           scale=c(4,.5), max.words=101, colors= brewer.pal(8,"Dark2"))
plot2 <- ggplot(d[1:42,], aes(label = word,
                              size=freq,   
                              color = factor(sample.int(10, nrow(d[1:42,]), replace =T)))) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 10) +
  ggtitle("Palabras más usadas en el programa de Briones") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = -2))
ggsave(filename = "Programa de Briones.png", plot = plot2)

hist2 <- ggplot(data = d[1:25,], aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(position="stack", stat="identity", show.legend = FALSE, fill = "darkturquoise") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  ggtitle("Palabras más usadas en el programa de Briones") +
  ylab("Frecuencia") +
  xlab("Palabras") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) 
ggsave(filename = "Hist de Briones.png", plot = hist2)
  
