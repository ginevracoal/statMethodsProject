library(ReadMe)
library(tidyverse)
#takes as input files created by amazon_lentgh_analysis.R
setwd("/home/doma/0dssc/smds/statMethodsProject/input_readme/")
data_raw=readRDS("../dataset/amazon_frasi_corte.rds")

library(tidyverse)
reviews=data_raw %>%
  select(reviewText,rating,reviewTime,number_words) %>%
  add_column(SentenceId=1:length(data_raw$reviewText))

n <- 20000
#n = length(reviews$SentenceId)
# create text files with proper filenames (numbers)
unlink("../input_readme/*")
data=reviews
apply(data[1:n,], 1, 
      function(x) write.table(data.frame(x[1]), 
                              file = gsub(" ","", paste("../input_readme/",x[5],sep="")), row.names = FALSE, col.names = FALSE))



# create control file with same naming conventions as rotten tomatoes
#training_perc=1000/n
training_perc=0.5

data1 <- data[1:n,] %>% 
  rename(Sentiment=rating)%>%
  select(SentenceId, Sentiment) %>% 
  rename(filename = SentenceId) %>% 
  rename(truth = Sentiment) %>% 
  add_column(trainingset = rbinom(n, 1, training_perc))

# #######
# head(data1)
# classes=data1%>%
#   select(truth,trainingset)%>%
#   group_by(truth,trainingset)%>%
#   summarise(total=n())%>%
#   mutate(percentage=total/sum(total))


####
write.csv(data1, file = "../input_readme/control.txt", row.names = FALSE)    

#analisi con readme
output <- undergrad(sep = ',') 
preprocess <- preprocess(output)
results <- readme(undergradlist=preprocess,boot.se = FALSE , nboot = 150)

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
    title = "Computed vs fitted values for Amazon music dataset, frasi corte",
    subtitle = paste("total number of elements:",n,",","of which training:",1000))+
  # geom_smooth(method=lm)+
  xlab("Real value")+
  ylab("Computed value")+
  theme_minimal()+
  geom_abline(slope = 1,intercept = 0)

plot(p)
############################
