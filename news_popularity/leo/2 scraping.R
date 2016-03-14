source("1 kaggle_load.R")

####################
# extract number of shares
###################

shares<-rep(NA, nrow(df2))

urls<-as.vector(as.character(df2$url))

#scrape
shares.full<-rep(NA,nrow(df2))

u<-sapply(1:nrow(df2), function(i) {
  cat(as.character(i))
  html<-urls[i]
  read<-tryCatch(read_html(html), error=function(e) NULL)
  
  if (is.null(read)==TRUE){
    shares[i]<<-"NULL"
  } else {
    extract<-html_nodes(read, css=".total-shares")
    
    if (length(as.character(extract))==0){
      shares[i]<<-"NULL"
    } else{
      shares[i]<<-as.character(html_nodes(extract, "em"))
    }}
})

#write.csv(shares.full, "sharesfull.csv")

df3<-cbind(df2,shares.full)
df3$shares.full<-as.character(df3$shares.full)

#create a vector to query function based on number of characters
numb.char<-sapply(1:nrow(df3), function(x){nchar(df3$shares.full)[x]})
df3<-cbind(df3, numb.char)

df3$shares.full<-as.character(df3$shares.full)

#> table(df4$numb.char)
#
#4       10    11    12    13    30    31 
#4317     5    69 10009 23240  1945    59 


#clean no. of shares
p.10<-which(df3$numb.char=="10", arr.ind=TRUE)
df3$shares.full[p.10]<-"0"

p.11<-which(df3$numb.char=="11", arr.ind=TRUE)
df3.11<-df3[which(df3$numb.char==11),]
df3.11$shares.full<-substr(df3.11$shares.full,5,6)
df3$shares.full[p.11]<-df3.11$shares.full

p.12<-which(df3$numb.char=="12", arr.ind=TRUE)
df3.12<-df3[which(df3$numb.char==12),]
df3.12$shares.full<-substr(df3.12$shares.full,5,7)
df3$shares.full[p.12]<-df3.12$shares.full

p.13<-which(df3$numb.char=="13", arr.ind=TRUE)
df3.13<-df3[which(df3$numb.char==13),]
df3.13$shares.full<-substr(df3.13$shares.full,5,7)
df3.13$shares.full<-sapply(strsplit(df3.13$shares.full, split='.', fixed=TRUE), function(x) (paste(x[1], x[2], "00", sep="")))
df3$shares.full[p.13]<-df3.13$shares.full

p.30<-which(df3$numb.char=="30", arr.ind=TRUE)
df3.30<-df3[which(df3$numb.char==30),]
df3.30$shares.full<-substr(df3.30$shares.full,22,24)
df3.30$shares.full<-sapply(strsplit(df3.30$shares.full, split='.', fixed=TRUE), function(x) (paste(x[1], x[2], "00", sep="")))

storage<-rep(NA, nrow(df3.30))
for (i in 1:nrow(df3.30)){
  if (substring(df3.30$shares.full[i], 1, 1)=="0"){
    storage[i]<-substring(df3.30$shares.full[i],2)
  } else {storage[i]<-df3.30$shares.full[i]}
}

storage2<-rep(NA, nrow(df3.30))
for (i in 1:nrow(df3.30)){
  if (substring(df3.30$shares.full[i], 1, 1)==","){
    storage2[i]<-"1000"
  } else {storage2[i]<-storage[i]}
}

df3.30$shares.full<-storage2
df3$shares.full[p.30]<-df3.30$shares.full

p.31<-which(df3$numb.char=="31", arr.ind=TRUE)
df3.31<-df3[which(df3$numb.char==31),]
df3.31$shares.full<-substr(df3.31$shares.full,21,25)
df3.31$shares.full<-sapply(strsplit(df3.31$shares.full, split='.', fixed=TRUE), function(x) (paste(x[1], x[2], "00", sep="")))
df3$shares.full[p.31]<-df3.31$shares.full

table(nchar(df3$shares.full))

df3$shares.full<-as.numeric(df3$shares.full)
#write.csv(df3, "df3_fullshares.csv")

#check NAs
table(is.na(df3$shares.full)) #4317 NA
table(is.na(df3$shares.full[30001:nrow(df3)])) #30 NA








######################
#extract author name
######################
shares2<-rep(NA, nrow(df2))

urls2<-as.vector(as.character(df2$url))

u<-sapply(1:nrow(df2), function(i) {
  cat(as.character(i))
  html<-urls2[i]
  read<-tryCatch(read_html(html), error=function(e) NULL)
  
  if (is.null(read)==TRUE){
    shares2[i]<<-"NULL"
  } else {
    extract<-html_nodes(read, css=".author_name")
    
    if (length(as.character(extract))==0){
      shares2[i]<<-"NULL"
    } else{
      shares2[i]<<-as.character(extract)
    }}
})

write.csv(shares2, "#data sets/author_name.csv")

author<-read.csv("#data sets/author_name.csv", colClasses = "character")
author[,1]<-NULL
author_final<-sapply(1:nrow(author), function(x){
  if(author[x,]=="NULL"){
    print("NULL")
  } else {
    split<-strsplit(author[x,], split=" ")
    concat<-paste(unlist(split)[-c(1,2)], collapse=" ")
    print(substr(concat,1,nchar(concat)-7))  
  }
})

length(unique(author_final)) #340 unique authors, including NULL
length(which(author_final=="NULL")) #6202 NULL
length(which(author_final[30001:length(author_final)]=="NULL")) #1555 NULL in test set

df3$author<-author_final

write.csv(df3,"#data sets/df3_complete.csv")