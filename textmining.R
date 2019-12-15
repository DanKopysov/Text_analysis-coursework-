library(readxl)
data <- read_excel("C:/Users/xiaomi/Downloads/news_for_analysis.xlsx")

head(data)
news<- c(data$title)

head(news)

install.packages("qdap")
install.packages("rJava")
library(rJava)
library(qdap)

term_count <- freq_terms(news, 10)
plot(term_count)
Encoding(news)

install.packages("tm")
library(tm)

news_corpus<- VCorpus(VectorSource(news), readerControl = list(reader = readPlain, language = "ru", load = T))
content(news_corpus[[1]])
meta(news_corpus[[1]])
stopwords("russian")
corpus1 <- VCorpus(VectorSource(news))




clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, с(stopwords("russian"), "санкция", "из-за", "санкционный", ''))
  corpus <- tm_map(corpus, removePunctuation) 
  corpus <- tm_map(corpus, content_transformer(mystem))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}



cleaned_news<-clean_corpus(news_corpus)
content(cleaned_news[[3]])


mystem <- function(doc) {
  library(stringr)
  sdoc <- system('mystem/mystem -nl -e CP1251', intern=T, input=doc)
  sdoc <- str_replace(sdoc, '\\|.*$', '')
  sdoc <- str_replace(sdoc, '\\?', '')
  sdoc <- paste(sdoc, collapse=" ")
  attributes(sdoc) <- attributes(doc)
  sdoc
}

news_set_stem <- tm_map(news_corpus, content_transformer(mystem))
news_set_stem <- tm_map(news_set_stem, removeWords, c(stopwords("russian"), "санкция", "из-за", "санкционный", 'называть'))

#Proverka na subsete

news_subset <- c(news[1:10])
news_subset
news_subset_corp <- VCorpus(VectorSource(news_subset))
news_subset_stem <- clean_corpus(news_subset_corp)


content(news_subset_stem[[1]])
content(news_subset_stem[[1]])

Encoding(news_subset)

##### TDM

tdm <- TermDocumentMatrix(news_set_stem)
tdm_m <- as.matrix(tdm)
term_frequency <- rowSums(tdm_m)
term_frequency<-sort(term_frequency, decreasing = TRUE)
term_frequency[1:50]
##### BARCHART
barplot(term_frequency[1:20],  col = 'tan', las = 2)

#####CLOUD
install.packages("wordcloud")
library(wordcloud)
terms_vec <- names(term_frequency)
ord_freqs <- wordcloud(terms_vec,term_frequency,  max.words = 40, colors = color_pal)
wordcloud

install.packages("viridisLite")
library(viridisLite)
# Select 5 colors (viridisLite package)
color_pal <- cividis(7)


#Associations
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
associations <- findAssocs(tdm, 'россия', 0.2)
associations1 <- findAssocs(tdm, 'против', 0.1)
associations2 <- findAssocs(tdm, 'сша', 0.1)
associations3 <- findAssocs(tdm, 'новый', 0.1)
associations4 <- findAssocs(tdm, 'вводить', 0.1)

associations_df <- list_vect2df(associations, col2 = "word", col3 = "score")
ggplot1 <- ggplot(associations_df, aes(score, word)) + 
  geom_point(size = 5, color = 'blue') + 
  theme_gdocs()
p <- ggplot1 + labs(title = '"Russia" word correlation')

associations_df2 <- list_vect2df(associations1, col2 = "word", col3 = "score")
ggplot21 <- ggplot(associations_df2, aes(score, word)) + 
  geom_point(size = 5, color = 'blue') + 
  theme_gdocs()
p2 <- ggplot21 + labs(title = '"Against" word correlation')

associations_df3 <- list_vect2df(associations2, col2 = "word", col3 = "score")
ggplot31 <- ggplot(associations_df3, aes(score, word)) + 
  geom_point(size = 5, color = 'blue') + 
  theme_gdocs()
p3<- ggplot31 + labs(title = '"USA" word correlation')


associations_df4 <- list_vect2df(associations3, col2 = "word", col3 = "score")
ggplot41 <- ggplot(associations_df4, aes(score, word)) + 
  geom_point(size = 5, color = 'blue') + 
  theme_gdocs()
p4 <- ggplot41 + labs(title = '"New" word correlation')

associations_df5 <- list_vect2df(associations4, col2 = "word", col3 = "score")
ggplot51 <- ggplot(associations_df5, aes(score, word)) + 
  geom_point(size = 5, color = 'blue') + 
  theme_gdocs()
p5<- ggplot51 + labs(title = '"Implement" word correlation')


####Tokenization
installed.packages("RWeka")
library(RWeka)
library(textmineR)
tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}
bigram_dtm <- DocumentTermMatrix(news_set_stem, control = list(tokenize = tokenizer))
bigram_dtm_m <- as.matrix(bigram_dtm)
freq_bigram <- colSums(bigram_dtm_m)
bi_words <- names(freq_bigram)
bigram_cloud <- wordcloud(bi_words, freq, max.words = 20, colors = color_pal)

####DENDROGRAM
dim(tdm)
tdm_dend <- removeSparseTerms(tdm, sparse = 0.965)
tdm_dend_m <- as.matrix(tdm_dend)
tdm_dist <- dist(tdm_dend_m)
hc <- hclust(tdm_dist)
plot(hc)
hcd <- as.dendrogram(hc)
labels (hcd)
