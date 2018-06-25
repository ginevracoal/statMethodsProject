#vediamo di integrare tweets e readme
library(tidyverse)
tweets=read_csv("../dataset/train_tweet_binario.csv")
setwd("/home/doma/0dssc/smds/statMethodsProject/input_readme/")



#contiamo le parole nei testi, dirty trick, conto gli spazi e aggiungo 1
tweets=tweets%>%
  mutate(number_words= (str_count(SentimentText,pattern = " ")+1) )
#plot dei risultati
ggplot(tweets,aes(number_words))+
  geom_histogram()+
  ggtitle(" words count")+
  theme_minimal()

#preanalisi dimensione minima del set----
classes=tweets%>%
  select(Sentiment)%>%
  group_by(Sentiment)%>%
  summarise(total=n())%>%
  mutate(percentage=total/sum(total))
data=tweets
#n = length(data$ItemID)
n=30000
# create text files with proper filenames (numbers)
unlink("../input_readme/*")

apply(data[1:n,], 1, 
      function(x) write.table(data.frame(x[3]), 
                              file = gsub(" ","", paste("../input_readme/",x[1],sep="")), row.names = FALSE, col.names = FALSE))


    # create control file with same naming conventions as rotten tomatoes
    #training_perc=1000/n
    training_perc=2000/n
    
    data1 <- data[1:n,] %>% 
      #rename(Sentiment=rating)%>%
      select(ItemID, Sentiment) %>% 
      rename(filename = ItemID) %>% 
      rename(truth = Sentiment) %>%
      add_column(trainingset = rbinom(n, 1, training_perc))
    
    
    
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
    
    
 