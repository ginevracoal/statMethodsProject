library(tidyverse)
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