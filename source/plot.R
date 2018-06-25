library(tidyverse)
library(ggplot2)

plot <- function(results){
  valori_fit = results$est.CSMF
  valori_veri = results$true.CSMF
  #converto in dataframe
  valori_fit = as_tibble(as.list(valori_fit))
  valori_veri = as_tibble(as.list(valori_veri))
  valori_plot=gather(valori_fit,type)
  valori_plot_veri=gather(valori_veri,type)
  #typeof(results$est.CSMF)
  valori_plot_veri
  valori_plot
  valori_tot=inner_join(valori_plot,valori_plot_veri,by="type")

  ggplot(data=valori_tot,aes(x=value.x,y=value.y))+
  geom_point()+
  ggtitle(" Computed vs fitted values")+
  geom_smooth(method=lm)+
  xlab("Real value")+
  ylab("Computed value")+
  theme_minimal()
}

plot_se <- function(results){
  valori_fit = results$est.CSMF
  valori_veri = results$true.CSMF
  sd_fit = results$CSMF.se
  #converto in dataframe
  valori_fit = as_tibble(as.list(valori_fit))
  valori_veri = as_tibble(as.list(valori_veri))
  sd_fit = as_tibble(as.list(sd_fit))
  valori_plot=gather(valori_fit,type)
  valori_plot_veri=gather(valori_veri,type)
  valori_sd=gather(sd_fit,type)
  #typeof(results$est.CSMF)
  valori_plot_veri
  valori_plot
  valori_sd
  valori_tot=inner_join(valori_plot,valori_plot_veri,by="type",
                        suffix=c(".est",".real"))
  valori_tot=inner_join(valori_tot,valori_sd)
  valori_tot
  
  ggplot(data=valori_tot,aes(x=value.real,y=value.est,
                             label = value.est))+
    geom_point(aes(color=type))+
    geom_linerange(aes(ymin=value.est-value, ymax=value.est+value,
                       color=type)) +
    ggtitle(" Computed vs fitted values for Amazon music dataset")+
    ggplot2::labs(
      title = "Computed vs fitted values for Amazon music dataset",
      subtitle = paste("total number of elements:",n,"of which training:",100*training_perc,"%"))+
    # geom_smooth(method=lm)+
    xlab("Real value")+
    ylab("Computed value")+
    theme_minimal()+
    geom_abline(slope = 1,intercept = 0)
}