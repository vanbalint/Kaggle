#creating bag of words data for training data

news_popularity_training <- read.csv("news_popularity_training.csv")

news_popularity_training$url <- as.character(news_popularity_training$url)

library(SnowballC)
library(plyr)

# The 'title' variable is not really the title,
# but the relevant part of the url, which summarizes the article in a few words.
title <- ldply(strsplit(news_popularity_training$url, "/"))$V7
title_by_word <- strsplit(title, "-")
rm(title)
removestopwords <- function(vector_of_words){
    vector_of_words[!(vector_of_words %in% stopwords())]
}
title_by_word <- llply(title_by_word, removestopwords)
title_by_word_stemmed <- llply(title_by_word, wordStem)
rm(title_by_word)
every_word <- unlist(title_by_word_stemmed)
words <- table(every_word)
rm(every_word)
frequent_words <- words[words>5]
rm(words)
wordcount <- matrix(NA, nrow(news_popularity_training), length(frequent_words))
for(obs in 1:nrow(news_popularity_training)){
    for(word in 1:length(frequent_words)){
        wordcount[obs, word] <- sum(title_by_word_stemmed[[obs]]==frequent_words[word])
    }
}
rm(title_by_word_stemmed)
news_popularity_training_and_wordcount <- cbind(news_popularity_training, wordcount)
rm(wordcount)
news_popularity_training_and_wordcount <- news_popularity_training_and_wordcount[,-1]

write.csv(news_popularity_training_and_wordcount, "news_popularity_training_and_wordcount.csv")
rm(news_popularity_training_and_wordcount)
rm(news_popularity_training)

#doing the same for test data

news_popularity_test<- read.csv("news_popularity_test.csv")
news_popularity_test$url <- as.character(news_popularity_test$url)
title <- ldply(strsplit(news_popularity_test$url, "/"))$V7
title_by_word <- strsplit(title, "-")
rm(title)
title_by_word <- llply(title_by_word, removestopwords)
title_by_word_stemmed <- llply(title_by_word, wordStem)
rm(title_by_word)

wordcount <- matrix(NA, nrow(news_popularity_test), length(frequent_words))
for(obs in 1:nrow(news_popularity_test)){
    for(word in 1:length(frequent_words)){
        wordcount[obs, word] <- sum(title_by_word_stemmed[[obs]]==frequent_words[word])
    }
}
rm(title_by_word_stemmed)
news_popularity_test_and_wordcount <- cbind(news_popularity_test, wordcount)
rm(wordcount)

write.csv(news_popularity_test_and_wordcount, "news_popularity_test_and_wordcount.csv")
# title_corpus <- Corpus(VectorSource(title_by_word))
# title_corpus <- tm_map(title_corpus, PlainTextDocument)
# title_corpus <- tm_map(title_corpus, removePunctuation)
# title_corpus <- tm_map(title_corpus, removeWords, stopwords('english'))
# title_corpus <- tm_map(title_corpus, stemDocument)
# wordcloud(title_corpus, max.words = 100, random.order = FALSE)

