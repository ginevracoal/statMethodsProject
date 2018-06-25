#my tweet + readme
library(tidyverse)
setwd("/home/doma/0dssc/smds/statMethodsProject/input_readme/")
#Tidy for hashtag study
dataset=readRDS("../dataset/reduced_tweet_no_duplicati_stripped_text_separated.RDS")

dataset=dataset%>%
  filter(lang=="it")%>%
  filter(is_retweet==FALSE)%>%
  filter(is_quote==FALSE)%>%
  select(status_id,created_at,user_id,text,hashtags)

#sottoinsiemi con hashtag singoli, tuttavia non sono post unici
iostoconmattarella=dataset%>%
  filter(hashtags=="iostoconmattarella")

mattarelladimettiti=dataset%>%
  filter(hashtags=="mattarelladimettiti")

impeachmentmattarella=dataset%>%
  filter(hashtags=="impeachmentmattarella")

impeachment=dataset%>%
  filter(hashtags=="impeachment")

mattarella=dataset%>%
  filter(hashtags=="mattarella")
#filtriamo in modo tale da avere solo post con singolo hashtag

promattarella=anti_join(iostoconmattarella,mattarelladimettiti,by="user_id")%>%
  anti_join(impeachment,by="user_id")%>%
  anti_join(impeachmentmattarella,by="user_id")

contra_mattarella1=anti_join(mattarelladimettiti,iostoconmattarella,by="user_id")
contra_mattarella2=anti_join(impeachment,iostoconmattarella,by="user_id")
contra_mattarella3=anti_join(impeachmentmattarella,iostoconmattarella,by="user_id")

contra_mattarella=rbind(contra_mattarella1,contra_mattarella2,contra_mattarella3)
rm(iostoconmattarella,dataset,contra_mattarella1,contra_mattarella2,contra_mattarella3,impeachment,mattarelladimettiti,impeachmentmattarella)

#prova che tutto abbia funzionato
inner_join(promattarella,contra_mattarella,by="status_id")

#filtriamo nel periodo hot
# promattarella=promattarella%>%
#   filter(created_at>="2018-05-26"& created_at<="2018-05-28")
# 
# contra_mattarella=contra_mattarella%>%
#   filter(created_at>="2018-05-26"& created_at<="2018-05-28")


megatrain=rbind(promattarella,contra_mattarella,mattarella)

megatrain$status_id_prog=1:length(megatrain$status_id)
#codifico con 1 classe pro mattarella e con 0 classo contro
megatrain=megatrain%>%
  mutate(class=case_when(hashtags=="iostoconmattarella" ~ 1,
                         hashtags=="impeachment" ~ 0,
                         hashtags=="impeachmentmattarella" ~ 0,
                        hashtags=="mattarelladimettiti" ~ 0,
                        hashtags=="mattarella" ~ 2))

#contiamo le parole nei testi, dirty trick, conto gli spazi e aggiungo 1
megatrain=megatrain%>%
  mutate(number_words= (str_count(text,pattern = " ")+1) )%>%
  distinct(status_id,.keep_all=TRUE)

#plot dei risultati
megatrain%>%
  filter(number_words<50)%>%
ggplot(aes(number_words))+
  geom_histogram(bins=50)+
  ggtitle(" words count")+
  theme_minimal()

data=megatrain
n = length(data$status_id)
#n=30000
# create text files with proper filenames (numbers)
unlink("../input_readme/*")

apply(data[1:n,], 1, 
      function(x) write.table(data.frame(x[4]), 
                              file = gsub(" ","", paste("../input_readme/",x[6],sep="")), row.names = FALSE, col.names = FALSE))


# create control file with same naming conventions as rotten tomatoes
#training_perc=1000/n
training_perc=1000/n

nnn= dim( (filter(data,class==0 | class==1)))[1]
data1 <- data[1:n,] %>% 
  filter(class==0 | class==1)%>%
  #rename(class=rating)%>%
  select(status_id_prog, class) %>% 
  rename(filename = status_id_prog) %>% 
  rename(truth = class) %>%
  add_column(trainingset = rbinom(nnn, 1, training_perc))

#da qua in poi nn cambia niente rispetto a prima in quanto ho fatto il doppio filter




# #######
# head(data1)
classes=data1%>%
  select(truth,trainingset)%>%
  group_by(truth,trainingset)%>%
  summarise(total=n())%>%
  mutate(percentage=total/sum(total))


####
write.csv(data1, file = "../input_readme/control.txt", row.names = FALSE)    

#analisi con readme
output <- undergrad(sep = ',',stem = FALSE) 
preprocess <- preprocess(output)
results <- readme(undergradlist=preprocess,boot.se = FALSE , nboot = 100)

################plotting mod no bootstrap

valori_fit = results$est.CSMF
valori_veri = results$true.CSMF
#sd_fit = results$CSMF.se
#converto in dataframe
valori_fit = as_tibble(as.list(valori_fit))
valori_veri = as_tibble(as.list(valori_veri))
#sd_fit = as_tibble(as.list(sd_fit))
valori_plot=gather(valori_fit,type)
valori_plot_veri=gather(valori_veri,type)
#valori_sd=gather(sd_fit,type)
#typeof(results$est.CSMF)
valori_plot_veri
valori_plot
#valori_sd
valori_tot=inner_join(valori_plot,valori_plot_veri,by="type",suffix=c(".est",".real"))
#valori_tot=inner_join(valori_tot,valori_sd)
#valori_tot

p=ggplot(data=valori_tot,aes(x=value.real,y=value.est,label = value.est))+
  geom_point(aes(color=type))+
  #  geom_linerange(aes(ymin=value.est-value, ymax=value.est+value,color=type)) +
  # ggtitle(" Computed vs fitted values for Amazon music dataset, frasi corte")+
  ggplot2::labs(
    title = "Computed vs fitted values for Amazon music dataset, frasi extra corte",
    subtitle = paste("total number of elements:",n,",","of which training:",round(training_perc*n)))+
  # geom_smooth(method=lm)+
  xlab("Real value")+
  ylab("Computed value")+
  theme_minimal()+
  geom_abline(slope = 1,intercept = 0)

plot(p)
############################


# ################plotting mod  bootstrap
# 
# valori_fit = results$est.CSMF
# valori_veri = results$true.CSMF
# sd_fit = results$CSMF.se
# #converto in dataframe
# valori_fit = as_tibble(as.list(valori_fit))
# valori_veri = as_tibble(as.list(valori_veri))
# sd_fit = as_tibble(as.list(sd_fit))
# valori_plot=gather(valori_fit,type)
# valori_plot_veri=gather(valori_veri,type)
# valori_sd=gather(sd_fit,type)
# #typeof(results$est.CSMF)
# valori_plot_veri
# valori_plot
# valori_sd
# valori_tot=inner_join(valori_plot,valori_plot_veri,by="type",suffix=c(".est",".real"))
# valori_tot=inner_join(valori_tot,valori_sd)
# #valori_tot
# 
# p=ggplot(data=valori_tot,aes(x=value.real,y=value.est,label = value.est))+
#   geom_point(aes(color=type))+
#     geom_linerange(aes(ymin=value.est-value, ymax=value.est+value,color=type)) +
#   # ggtitle(" Computed vs fitted values for Amazon music dataset, frasi corte")+
#   ggplot2::labs(
#     title = "Computed vs fitted values for Amazon music dataset, frasi extra corte",
#     subtitle = paste("total number of elements:",n,",","of which training:",round(training_perc*n)))+
#   # geom_smooth(method=lm)+
#   xlab("Real value")+
#   ylab("Computed value")+
#   theme_minimal()+
#   geom_abline(slope = 1,intercept = 0)
# 
# plot(p)
# ############################


#proporzione a mano

sum(megatrain$class==1)/(sum(megatrain$class==0)+sum(megatrain$class==1))
