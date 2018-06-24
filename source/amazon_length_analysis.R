#json import---- 

set.seed(1)
library(ReadMe)
library(rjson)
library(tidyverse)
library(jsonlite)
#set right environment, run only once!
oldwd <- getwd()
setwd("/home/doma/0dssc/smds/statMethodsProject/input_readme/")

json_data = stream_in(file("../dataset/CDs_and_Vinyl_5.json",open="r"))
#take only what you need
reviews=json_data %>%
  select(reviewText,overall,reviewTime) %>%
  add_column(SentenceId=1:length(json_data$reviewText))%>%
  rename(rating=overall)


#class distribution plot----
plot_distribution_ratings=function(reviews){
classes=reviews%>%
  select(rating)%>%
  group_by(rating)%>%
  summarise(total=n())%>%
  mutate(percentage=total/sum(total))

p=ggplot(classes,aes(rating,total,fill=as.factor(rating)))+
  geom_col()+
  ggtitle(" Ratings count")+
  labs(fill="Rating")+
  theme_minimal()
return(p)
}

plot_distribution_ratings(reviews)
#text length distribution----
#contiamo le parole nei testi, dirty trick, conto gli spazi e aggiungo 1
reviews=reviews%>%
  mutate(number_words= (str_count(reviewText,pattern = " ")+1) )
#plot dei risultati
ggplot(reviews,aes(number_words))+
  geom_histogram()+
  ggtitle(" words count")+
  theme_minimal()

corte=reviews%>%
  filter(number_words<=50 & number_words>=26)

extra_corte=reviews%>%
  filter(number_words<=25 & number_words>=1)

ggplot(corte,aes(number_words))+
  geom_histogram(bins=50)+
  ggtitle(" words count")+
  theme_minimal()


lunghe=reviews%>%
  filter(number_words>=51 & number_words<=150)

extra_lunghe=reviews%>%
  filter(number_words>=151 & number_words<=1000)


ggplot(lunghe,aes(number_words))+
  geom_histogram(bins=50)+
  ggtitle(" words count")+
  theme_minimal()


plot_distribution_ratings(lunghe)
plot_distribution_ratings(corte)

#check correlation length-rating----
ggplot(reviews,aes(x=as.factor(rating),y=number_words))+
  geom_boxplot()+
  ggtitle("Relation between number of words an rating all dataset")+
  xlab("Rating")+
  ylab("Number of words")+
  theme_minimal()

ggplot(extra_corte,aes(x=as.factor(rating),y=number_words))+
  geom_boxplot()+
  ggtitle("Relation between number of words an rating extra short")+
  xlab("Rating")+
  ylab("Number of words")+
  theme_minimal()

ggplot(corte,aes(x=as.factor(rating),y=number_words))+
  geom_boxplot()+
  ggtitle("Relation between number of words an rating short")+
  xlab("Rating")+
  ylab("Number of words")+
  theme_minimal()

ggplot(lunghe,aes(x=as.factor(rating),y=number_words))+
  geom_boxplot()+
  ggtitle("Relation between number of words an rating long")+
  xlab("Rating")+
  ylab("Number of words")+
  theme_minimal()

ggplot(extra_lunghe,aes(x=as.factor(rating),y=number_words))+
  geom_boxplot()+
  ggtitle("Relation between number of words an rating extra long")+
  xlab("Rating")+
  ylab("Number of words")+
  theme_minimal()
#no relation!
#save results for further analysis

saveRDS(extra_corte,"../dataset/amazon_frasi_extra_corte.rds")
saveRDS(corte,"../dataset/amazon_frasi_corte.rds")
saveRDS(lunghe,"../dataset/amazon_frasi_lunghe.rds")
saveRDS(extra_lunghe,"../dataset/amazon_frasi_extra_lunghe.rds")