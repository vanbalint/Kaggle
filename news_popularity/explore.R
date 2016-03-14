setwd("D:/Tanulas/Kaggle/news_popularity")

library(ggplot2)
library(RColorBrewer)

news_popularity_training <- read.csv("news_popularity_training.csv")
news_popularity_training$popularity <- as.factor(news_popularity_training$popularity)
news_popularity_training <- news_popularity_training[-which(news_popularity_training$n_non_stop_words>2),]

hist(news_popularity_training$timedelta[news_popularity_training$popularity==1])
hist(news_popularity_training$timedelta[news_popularity_training$popularity==2])
hist(news_popularity_training$timedelta[news_popularity_training$popularity==3])
hist(news_popularity_training$timedelta[news_popularity_training$popularity==4])
hist(news_popularity_training$timedelta[news_popularity_training$popularity==5])
table(news_popularity_training$popularity)


np_boxplot <- function(indata, inx, iny){
    p <- ggplot(indata, 
                aes_q(x = as.name(names(indata)[inx]), 
                      y = as.name(names(indata)[iny])))
    p + geom_boxplot()
}
for(i in 3:61){
    filename <- paste("explore/bp_", names(news_popularity_training[i]), ".jpg", sep = "")
    jpeg(file = filename)
    print(np_boxplot(news_popularity_training, 62, i))
    dev.off()
}
#I know that for binary variables these boxplots do not show anything

for(i in 3:61){
    filename <- paste("explore/kw_avg_avg_X_", names(news_popularity_training[i]), ".jpg", sep = "")
    jpeg(file = filename)
    print( ggplot(news_popularity_training, 
                  aes_q(x = as.name(names(news_popularity_training)[29]), 
                        y = as.name(names(news_popularity_training)[i]),
                        color = as.name("popularity"))) +
               theme_bw() +
                stat_ellipse()+
               scale_fill_brewer(palette="Set1")
    )
    dev.off()
}

news_popularity_training$popularity <- as.numeric(news_popularity_training$popularity)

for(i in 4:61){
    filename <- paste("explore/timedelta_X_", names(news_popularity_training[i]), ".jpg", sep = "")
    jpeg(file = filename)
    print( ggplot(news_popularity_training, 
                  aes_q(x = as.name(names(news_popularity_training)[3]), 
                        y = as.name(names(news_popularity_training)[i]),
                        color = as.name("popularity"))) +
               theme_bw() +
               stat_ellipse()
    )
    dev.off()
}

for(i in 3:61){
    filename <- paste("explore/kw_max_avg_X_", names(news_popularity_training[i]), ".jpg", sep = "")
    jpeg(file = filename)
    print( ggplot(news_popularity_training, 
                  aes_q(x = as.name(names(news_popularity_training)[28]), 
                        y = as.name(names(news_popularity_training)[i]),
                        color = as.name("popularity"))) +
               theme_bw() +
               stat_ellipse()
    )
    dev.off()
}

for(i in 3:61){
    filename <- paste("explore/LDA4_X_", names(news_popularity_training[i]), ".jpg", sep = "")
    jpeg(file = filename)
    print( ggplot(news_popularity_training, 
                  aes_q(x = as.name(names(news_popularity_training)[45]), 
                        y = as.name(names(news_popularity_training)[i]),
                        color = as.name("popularity"))) +
               theme_bw() +
               stat_ellipse()
    )
    dev.off()
}